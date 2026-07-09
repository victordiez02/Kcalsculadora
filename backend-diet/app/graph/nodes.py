"""Nodos del grafo: un nodo = una función.

Planner y Chef llaman al LLM con salida estructurada; Retriever, Ajustador y
Validator son deterministas (RAG puro y aritmética). El LLM decide QUÉ se
come (reparto y platos); los gramos y las kcal/macros los calcula siempre
Python a partir del dataset. Los helpers `_` son funciones puras para poder
testearlos sin LLM ni vector store.
"""

from __future__ import annotations

import unicodedata
from functools import lru_cache
from typing import Literal

from langchain_openai import ChatOpenAI

from .. import constants as C
from ..rag.vectorstore import buscar
from ..schemas import (
    Comida,
    Desviacion,
    DietInput,
    IngredienteChef,
    IngredienteResuelto,
    Macros,
    MejorPlan,
    MenuChefLLM,
    PlatoChef,
    PlatoResuelto,
    Producto,
    ProductoPlan,
    RepartoComida,
    RepartoLLM,
)
from .cantidades import ajustar_cantidades
from .state import DietState


class SinCandidatosError(Exception):
    """No hay productos suficientes para montar algún plato con esos filtros."""


@lru_cache(maxsize=1)
def _llm() -> ChatOpenAI:
    return ChatOpenAI(model=C.OPENAI_MODEL)


def _normalizar_texto(s: str) -> str:
    """minúsculas y sin tildes, para comparar con `evitar`/`favoritos`."""
    s = unicodedata.normalize("NFD", s.lower())
    return "".join(ch for ch in s if unicodedata.category(ch) != "Mn")


def decidir_inicio(state: DietState) -> Literal["planner", "chef"]:
    """El reparto solo depende de la entrada: al generar varios menús para la
    semana, los siguientes reutilizan el del primero y entran por el Chef."""
    return "chef" if state.get("reparto") else "planner"


# ---------------------------------------------------------------- Planner


PLANNER_SYSTEM = (
    "Eres un nutricionista deportivo. Repartes las calorías y macros diarios "
    "de un usuario entre sus comidas del día de forma realista para la dieta "
    "española: la comida principal es la más energética, el desayuno moderado, "
    "los snacks (almuerzo/merienda) ligeros. Devuelves exclusivamente el "
    "reparto en el formato pedido, usando EXACTAMENTE los nombres de comida "
    "indicados y sin inventar comidas."
)


def _normalizar_reparto(comidas: list[RepartoComida], entrada: DietInput) -> list[RepartoComida]:
    """Reescala el reparto del LLM para que las sumas cuadren EXACTAMENTE
    con el objetivo diario (el LLM decide la forma, Python las cuentas)."""
    objetivo = {
        "kcal": entrada.kcal_objetivo,
        "proteinas": entrada.macros_objetivo.proteinas,
        "grasas": entrada.macros_objetivo.grasas,
        "carbohidratos": entrada.macros_objetivo.carbohidratos,
    }
    resultado = [c.model_copy() for c in comidas]
    for campo, total_obj in objetivo.items():
        suma = sum(getattr(c, campo) for c in resultado)
        if suma <= 0:
            # Reparto degenerado: repartir uniforme.
            for c in resultado:
                setattr(c, campo, round(total_obj / len(resultado), 1))
            continue
        factor = total_obj / suma
        for c in resultado:
            setattr(c, campo, round(getattr(c, campo) * factor, 1))
    return resultado


def planner(state: DietState) -> dict:
    entrada = state["entrada"]
    nombres = C.NOMBRES_COMIDAS[entrada.num_comidas]
    feedback = state.get("feedback", "")
    partes = [
        f"Objetivo diario: {entrada.kcal_objetivo:.0f} kcal, "
        f"{entrada.macros_objetivo.proteinas:.0f} g proteína, "
        f"{entrada.macros_objetivo.grasas:.0f} g grasa, "
        f"{entrada.macros_objetivo.carbohidratos:.0f} g carbohidratos.",
        f"Fase del usuario: {entrada.objetivo} (mant=recomposición, def=definición, vol=volumen).",
        f"Comidas del día ({entrada.num_comidas}): {', '.join(nombres)}.",
        "Reparte kcal y macros entre esas comidas.",
    ]
    if feedback:
        partes.append(f"IMPORTANTE, feedback del intento anterior: {feedback}")
    salida: RepartoLLM = (
        _llm()
        .with_structured_output(RepartoLLM, method="json_schema")
        .invoke([("system", PLANNER_SYSTEM), ("human", "\n".join(partes))])
    )
    # Alinear nombres/número de comidas por si el LLM se desvía.
    por_nombre = {c.nombre: c for c in salida.comidas}
    comidas = [
        por_nombre.get(n, RepartoComida(nombre=n, kcal=0, proteinas=0, grasas=0, carbohidratos=0))
        for n in nombres
    ]
    return {"reparto": _normalizar_reparto(comidas, entrada), "feedback": ""}


# ------------------------------------------------------------------- Chef


CHEF_SYSTEM = (
    "Eres un chef y nutricionista deportivo. Diseñas un menú diario con "
    "PLATOS reales y apetecibles de cocina española/mediterránea sencilla, "
    "cocinables en casa: cada comida es UN plato con nombre (p. ej. «Salmón "
    "al horno con arroz y brócoli»), no una lista suelta de productos. Los "
    "ingredientes son alimentos genéricos de supermercado (pechuga de pollo, "
    "arroz, yogur natural…), sin marcas, cada uno con su categoría y una "
    "cantidad orientativa en gramos. Las cantidades exactas se ajustan "
    "después automáticamente: importa la PROPORCIÓN del plato, no la "
    "precisión. Cada plato debe poder cubrir los macros de su comida: incluye "
    "fuente de proteína y de carbohidrato cuando el objetivo lo pida. Usa "
    f"entre {C.INGREDIENTES_POR_PLATO_MIN} y {C.INGREDIENTES_POR_PLATO_MAX} "
    "ingredientes por plato, y EXACTAMENTE los nombres de comida indicados."
)


def _prompt_chef(state: DietState) -> str:
    entrada = state["entrada"]
    partes = ["Objetivo de cada comida:"]
    for r in state["reparto"]:
        partes.append(
            f"- {r.nombre}: {r.kcal:.0f} kcal, P {r.proteinas:.0f} g, "
            f"G {r.grasas:.0f} g, C {r.carbohidratos:.0f} g"
        )
    partes.append(
        f"Fase del usuario: {entrada.objetivo} (mant=recomposición, def=definición, vol=volumen)."
    )
    if entrada.favoritos:
        partes.append(
            f"Al usuario le encantan: {', '.join(entrada.favoritos)}. Inclúyelos donde encajen."
        )
    if entrada.evitar:
        partes.append(f"PROHIBIDO usar: {', '.join(entrada.evitar)}.")
    if entrada.intolerancias:
        labels = [C.INTOLERANCIAS[i]["label"] for i in entrada.intolerancias]
        partes.append(
            f"Intolerancias (no uses ingredientes que las contengan): {', '.join(labels)}."
        )
    partes.append("No repitas el ingrediente principal entre comidas del día.")
    platos_usados = state.get("platos_usados", [])
    if platos_usados:
        partes.append(
            "Estos platos ya se usaron en otros días de la semana, propón platos "
            f"DIFERENTES: {', '.join(platos_usados)}."
        )
    no_encontrados = state.get("no_encontrados", [])
    if no_encontrados:
        partes.append(
            f"Estos ingredientes NO están disponibles en {C.SUPERMERCADOS[entrada.supermercado]['label']}, "
            f"no los uses: {', '.join(no_encontrados)}."
        )
    feedback = state.get("feedback", "")
    if feedback:
        partes.append(f"IMPORTANTE, feedback del intento anterior: {feedback}")
    return "\n".join(partes)


def _alinear_platos(salida: MenuChefLLM, nombres: list[str]) -> list[PlatoChef]:
    """Un plato por comida, en el orden del reparto. Si el LLM omite una
    comida queda un plato vacío: el edge tras el Retriever lo detecta y
    reintenta el Chef."""
    por_comida = {p.comida: p for p in salida.platos}
    return [por_comida.get(n, PlatoChef(comida=n, nombre="", ingredientes=[])) for n in nombres]


def chef(state: DietState) -> dict:
    entrada = state["entrada"]
    salida: MenuChefLLM = (
        _llm()
        .with_structured_output(MenuChefLLM, method="json_schema")
        .invoke([("system", CHEF_SYSTEM), ("human", _prompt_chef(state))])
    )
    platos = _alinear_platos(salida, C.NOMBRES_COMIDAS[entrada.num_comidas])
    return {
        "platos_chef": platos,
        "intentos": state.get("intentos", 0) + 1,
        "feedback": "",
    }


# -------------------------------------------------------------- Retriever


def _pasa_salvaguarda_intolerancia(producto: Producto, intolerancias: list[str]) -> bool:
    """Filtro extra sobre el del vector store: en categorías de riesgo, un
    producto SIN alérgenos declarados solo pasa si su nombre lleva un
    marcador explícito ("sin lactosa", "sin gluten", bebida vegetal…)."""
    if producto.alergenos:
        # Declara alérgenos y el vector store ya excluyó los prohibidos.
        return True
    nombre = _normalizar_texto(producto.nombre)
    for iid in intolerancias:
        if producto.categoria in C.CATEGORIAS_RIESGO_INTOLERANCIA.get(iid, []):
            marcadores = C.MARCADORES_SIN_INTOLERANCIA.get(iid, [])
            if not any(_normalizar_texto(m) in nombre for m in marcadores):
                return False
    return True


def _filtrar_evitar(productos: list[Producto], evitar: list[str]) -> list[Producto]:
    terminos = [_normalizar_texto(e) for e in evitar]
    return [
        p
        for p in productos
        if not any(t in _normalizar_texto(f"{p.nombre} {p.marca}") for t in terminos)
    ]


def _elegir_producto(candidatos: list[Producto], entrada: DietInput) -> Producto | None:
    """El producto real para un ingrediente: entre los candidatos (ya
    ordenados por similitud) que pasan los filtros duros, el más popular."""
    candidatos = _filtrar_evitar(candidatos, entrada.evitar)
    candidatos = [p for p in candidatos if _pasa_salvaguarda_intolerancia(p, entrada.intolerancias)]
    if not candidatos:
        return None
    return max(candidatos, key=lambda p: p.popularidad)


def _resolver_ingrediente(ing: IngredienteChef, entrada: DietInput) -> Producto | None:
    candidatos = buscar(
        query=ing.nombre,
        supermercado=entrada.supermercado,
        categorias=[ing.categoria],
        intolerancias=entrada.intolerancias,
        n=C.N_RESULTADOS_POR_INGREDIENTE,
    )
    if not candidatos:
        # La categoría del Chef puede no cuadrar con la del dataset: reintento
        # sin filtro de categoría antes de darlo por no disponible.
        candidatos = buscar(
            query=ing.nombre,
            supermercado=entrada.supermercado,
            intolerancias=entrada.intolerancias,
            n=C.N_RESULTADOS_POR_INGREDIENTE,
        )
    return _elegir_producto(candidatos, entrada)


def retriever(state: DietState) -> dict:
    """Mapea cada ingrediente genérico del Chef a un producto real del súper
    (RAG puro, sin LLM). Los no disponibles se acumulan en `no_encontrados`
    para que el Chef no vuelva a proponerlos."""
    entrada = state["entrada"]
    platos: list[PlatoResuelto] = []
    no_encontrados = set(state.get("no_encontrados", []))
    for plato in state["platos_chef"]:
        resueltos: list[IngredienteResuelto] = []
        codes_usados: set[str] = set()
        for ing in plato.ingredientes:
            producto = _resolver_ingrediente(ing, entrada)
            if producto is None:
                no_encontrados.add(ing.nombre)
            elif producto.code not in codes_usados:
                # Dos ingredientes pueden resolver al mismo producto; con uno
                # basta, el ajustador compensa la cantidad.
                codes_usados.add(producto.code)
                resueltos.append(IngredienteResuelto(ingrediente=ing, producto=producto))
        platos.append(
            PlatoResuelto(comida=plato.comida, nombre=plato.nombre, ingredientes=resueltos)
        )
    return {"platos": platos, "no_encontrados": sorted(no_encontrados)}


def decidir_tras_retriever(state: DietState) -> Literal["ajustador", "chef"]:
    """Si algún plato se quedó sin ingredientes suficientes (no existen en el
    súper), se pide otro plato al Chef mientras queden intentos."""
    incompleto = any(len(p.ingredientes) < C.INGREDIENTES_POR_PLATO_MIN for p in state["platos"])
    if incompleto and state.get("intentos", 0) < C.MAX_INTENTOS:
        return "chef"
    return "ajustador"


# -------------------------------------------------------------- Ajustador


def _construir_comida(plato: PlatoResuelto, gramos: list[float]) -> Comida:
    """Comida final con kcal/macros REALES calculados desde el dataset."""
    productos = [
        ProductoPlan(
            ingrediente=r.ingrediente.nombre,
            nombre=r.producto.nombre,
            marca=r.producto.marca,
            cantidad_g=g,
            kcal=round(r.producto.kcal_100g * g / 100, 1),
            proteinas=round(r.producto.proteinas_100g * g / 100, 1),
            grasas=round(r.producto.grasas_100g * g / 100, 1),
            carbohidratos=round(r.producto.carbohidratos_100g * g / 100, 1),
        )
        for r, g in zip(plato.ingredientes, gramos, strict=True)
    ]
    return Comida(
        nombre=plato.comida,
        plato=plato.nombre,
        productos=productos,
        kcal=round(sum(p.kcal for p in productos), 1),
        macros=Macros(
            proteinas=round(sum(p.proteinas for p in productos), 1),
            grasas=round(sum(p.grasas for p in productos), 1),
            carbohidratos=round(sum(p.carbohidratos for p in productos), 1),
        ),
    )


def ajustador(state: DietState) -> dict:
    """Nodo determinista: resuelve los gramos de cada plato por mínimos
    cuadrados para cuadrar el reparto del Planner (ver cantidades.py)."""
    reparto = {r.nombre: r for r in state["reparto"]}
    plan: list[Comida] = []
    for plato in state["platos"]:
        if not plato.ingredientes:
            raise SinCandidatosError(
                f"No se pudo montar un plato para «{plato.comida}» con esas restricciones. "
                "Prueba con menos alimentos a evitar u otro supermercado."
            )
        gramos = ajustar_cantidades(
            productos=[r.producto for r in plato.ingredientes],
            orientativas_g=[r.ingrediente.cantidad_g for r in plato.ingredientes],
            objetivo=reparto[plato.comida],
        )
        plan.append(_construir_comida(plato, gramos))
    return {"plan": plan}


# -------------------------------------------------------------- Validator


def _calcular_desviacion(plan: list[Comida], entrada: DietInput) -> Desviacion:
    total_kcal = sum(c.kcal for c in plan)
    totales = {
        "proteinas": sum(c.macros.proteinas for c in plan),
        "grasas": sum(c.macros.grasas for c in plan),
        "carbohidratos": sum(c.macros.carbohidratos for c in plan),
    }
    objetivo_macros = entrada.macros_objetivo.model_dump()

    def pct(real: float, obj: float) -> float:
        return round((real - obj) / obj, 4) if obj > 0 else 0.0

    kcal_pct = pct(total_kcal, entrada.kcal_objetivo)
    macros_pct = {m: pct(totales[m], objetivo_macros[m]) for m in totales}
    dentro = abs(kcal_pct) <= C.TOLERANCIA_KCAL_PCT and all(
        abs(v) <= C.TOLERANCIA_MACRO_PCT for v in macros_pct.values()
    )
    return Desviacion(
        kcal_pct=kcal_pct,
        proteinas_pct=macros_pct["proteinas"],
        grasas_pct=macros_pct["grasas"],
        carbohidratos_pct=macros_pct["carbohidratos"],
        dentro_tolerancia=dentro,
    )


def _puntuacion(d: Desviacion) -> float:
    """Cuanto más baja, mejor."""
    return C.PESO_DESVIACION_KCAL * abs(d.kcal_pct) + (
        abs(d.proteinas_pct) + abs(d.grasas_pct) + abs(d.carbohidratos_pct)
    )


def _feedback_desviacion(d: Desviacion, entrada: DietInput) -> str:
    """Feedback numérico explícito para el prompt del reintento."""
    mensajes: list[str] = []
    pares = [
        ("kcal", d.kcal_pct, entrada.kcal_objetivo, C.TOLERANCIA_KCAL_PCT, "kcal"),
        (
            "proteína",
            d.proteinas_pct,
            entrada.macros_objetivo.proteinas,
            C.TOLERANCIA_MACRO_PCT,
            "g",
        ),
        ("grasa", d.grasas_pct, entrada.macros_objetivo.grasas, C.TOLERANCIA_MACRO_PCT, "g"),
        (
            "carbohidratos",
            d.carbohidratos_pct,
            entrada.macros_objetivo.carbohidratos,
            C.TOLERANCIA_MACRO_PCT,
            "g",
        ),
    ]
    for nombre, pct_val, objetivo, tolerancia, unidad in pares:
        if abs(pct_val) > tolerancia:
            delta = pct_val * objetivo
            exceso = "sobran" if delta > 0 else "faltan"
            mensajes.append(f"{exceso} {abs(delta):.0f} {unidad} de {nombre} ({pct_val:+.0%})")
    return (
        "El plan anterior quedó fuera de tolerancia: "
        + "; ".join(mensajes)
        + ". Cambia ingredientes o platos para corregirlo (las cantidades se ajustan solas: "
        "si falta un macro, añade un ingrediente rico en él)."
    )


def validator(state: DietState) -> dict:
    entrada = state["entrada"]
    desviacion = _calcular_desviacion(state["plan"], entrada)
    puntuacion = _puntuacion(desviacion)

    mejor = state.get("mejor")
    if mejor is None or puntuacion < mejor.puntuacion:
        mejor = MejorPlan(comidas=state["plan"], desviacion=desviacion, puntuacion=puntuacion)

    salida: dict = {"desviacion": desviacion, "mejor": mejor}
    if desviacion.dentro_tolerancia:
        return salida

    if state.get("intentos", 0) >= C.MAX_INTENTOS:
        # Se agotaron los intentos: devolver el MEJOR plan visto, marcado
        # como aproximado (nunca fallar en silencio).
        salida.update(plan=mejor.comidas, desviacion=mejor.desviacion, aproximado=True)
        return salida

    salida["feedback"] = _feedback_desviacion(desviacion, entrada)
    return salida


def decidir_tras_validar(state: DietState) -> Literal["fin", "chef", "planner"]:
    """Edge condicional tras el Validator."""
    desviacion = state["desviacion"]
    if desviacion is not None and desviacion.dentro_tolerancia:
        return "fin"
    if state.get("aproximado"):
        return "fin"
    if desviacion is not None and abs(desviacion.kcal_pct) > C.UMBRAL_REPLANIFICAR_PCT:
        return "planner"
    return "chef"
