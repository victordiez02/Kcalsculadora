"""Nodos del grafo: un nodo = una función.

Planner y Composer llaman al LLM con salida estructurada; Retriever y
Validator son deterministas (RAG puro y aritmética). Los helpers `_` son
funciones puras para poder testearlos sin LLM ni vector store.
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
    Macros,
    MejorPlan,
    PlanComposerLLM,
    Producto,
    ProductoPlan,
    RepartoComida,
    RepartoLLM,
)
from .state import DietState


class SinCandidatosError(Exception):
    """No hay productos suficientes para alguna comida con esos filtros."""


@lru_cache(maxsize=1)
def _llm() -> ChatOpenAI:
    return ChatOpenAI(model=C.OPENAI_MODEL)


def _normalizar_texto(s: str) -> str:
    """minúsculas y sin tildes, para comparar con `evitar`/`favoritos`."""
    s = unicodedata.normalize("NFD", s.lower())
    return "".join(ch for ch in s if unicodedata.category(ch) != "Mn")


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


def _seleccionar_candidatos(
    resultados: list[Producto],
    entrada: DietInput,
    favoritos_encontrados: list[Producto],
) -> list[Producto]:
    """Dedupe + filtros finales + priorización (favoritos primero, luego
    popularidad) + tope de candidatos."""
    vistos: set[str] = set()
    unicos: list[Producto] = []
    for p in favoritos_encontrados + resultados:
        if p.code not in vistos:
            vistos.add(p.code)
            unicos.append(p)
    unicos = _filtrar_evitar(unicos, entrada.evitar)
    unicos = [p for p in unicos if _pasa_salvaguarda_intolerancia(p, entrada.intolerancias)]

    favoritos_codes = {p.code for p in favoritos_encontrados}
    unicos.sort(key=lambda p: (p.code not in favoritos_codes, -p.popularidad))
    return unicos[: C.N_CANDIDATOS_POR_COMIDA]


def retriever(state: DietState) -> dict:
    entrada = state["entrada"]
    candidatos: dict[str, list[Producto]] = {}
    for reparto in state["reparto"]:
        comida = reparto.nombre
        categorias_comida = C.CATEGORIAS_POR_COMIDA[comida]

        resultados: list[Producto] = []
        for categoria in categorias_comida:
            resultados += buscar(
                query=f"{C.CATEGORIA_LABELS[categoria]} para {comida.lower()}",
                supermercado=entrada.supermercado,
                categorias=[categoria],
                intolerancias=entrada.intolerancias,
                n=C.N_RESULTADOS_POR_CATEGORIA,
            )

        favoritos_encontrados: list[Producto] = []
        for favorito in entrada.favoritos:
            favoritos_encontrados += [
                p
                for p in buscar(
                    query=favorito,
                    supermercado=entrada.supermercado,
                    categorias=categorias_comida,
                    intolerancias=entrada.intolerancias,
                    n=4,
                )
                if _normalizar_texto(favorito) in _normalizar_texto(f"{p.nombre} {p.marca}")
            ]

        seleccion = _seleccionar_candidatos(resultados, entrada, favoritos_encontrados)
        if len(seleccion) < C.PRODUCTOS_POR_COMIDA_MIN:
            raise SinCandidatosError(
                f"No hay productos suficientes en {entrada.supermercado} para «{comida}» "
                "con esas restricciones. Prueba con menos alimentos a evitar u otro supermercado."
            )
        candidatos[comida] = seleccion
    return {"candidatos": candidatos}


# --------------------------------------------------------------- Composer


COMPOSER_SYSTEM = (
    "Eres un nutricionista que monta menús con productos REALES de "
    "supermercado. Para cada comida eliges productos SOLO de su lista de "
    "candidatos (por su código exacto) y una cantidad en gramos para cada "
    "uno, de modo que las kcal y macros de la comida se acerquen lo máximo "
    "posible a su objetivo. Las cantidades deben ser realistas (una lata de "
    "atún ~80 g, un yogur ~125 g, pan 40-120 g, aceite 5-20 g…) y estar "
    f"entre {C.CANTIDAD_MIN_G} y {C.CANTIDAD_MAX_G} g, en múltiplos de "
    f"{C.REDONDEO_CANTIDAD_G}. Usa entre {C.PRODUCTOS_POR_COMIDA_MIN} y "
    f"{C.PRODUCTOS_POR_COMIDA_MAX} productos por comida y monta comidas "
    "coherentes (no mezcles p. ej. cereales de desayuno en la cena)."
)


def _redondear_cantidad(g: float) -> float:
    g = max(C.CANTIDAD_MIN_G, min(C.CANTIDAD_MAX_G, g))
    return round(g / C.REDONDEO_CANTIDAD_G) * C.REDONDEO_CANTIDAD_G


def _construir_plan(salida: PlanComposerLLM, candidatos: dict[str, list[Producto]]) -> list[Comida]:
    """Convierte la salida del LLM (codes + gramos) en comidas con kcal y
    macros REALES calculados desde el dataset. Ignora códigos que no estén
    entre los candidatos de esa comida (anti-alucinación)."""
    comidas: list[Comida] = []
    for nombre_comida, productos_comida in candidatos.items():
        por_code = {p.code: p for p in productos_comida}
        comida_llm = next((c for c in salida.comidas if c.nombre == nombre_comida), None)
        productos_plan: list[ProductoPlan] = []
        for item in comida_llm.items if comida_llm else []:
            producto = por_code.get(item.code)
            if producto is None:
                continue
            g = _redondear_cantidad(item.cantidad_g)
            productos_plan.append(
                ProductoPlan(
                    nombre=producto.nombre,
                    marca=producto.marca,
                    cantidad_g=g,
                    kcal=round(producto.kcal_100g * g / 100, 1),
                    proteinas=round(producto.proteinas_100g * g / 100, 1),
                    grasas=round(producto.grasas_100g * g / 100, 1),
                    carbohidratos=round(producto.carbohidratos_100g * g / 100, 1),
                )
            )
        comidas.append(
            Comida(
                nombre=nombre_comida,
                productos=productos_plan,
                kcal=round(sum(p.kcal for p in productos_plan), 1),
                macros=Macros(
                    proteinas=round(sum(p.proteinas for p in productos_plan), 1),
                    grasas=round(sum(p.grasas for p in productos_plan), 1),
                    carbohidratos=round(sum(p.carbohidratos for p in productos_plan), 1),
                ),
            )
        )
    return comidas


def _prompt_composer(state: DietState) -> str:
    entrada = state["entrada"]
    partes: list[str] = []
    for reparto in state["reparto"]:
        partes.append(
            f"\n## {reparto.nombre} — objetivo: {reparto.kcal:.0f} kcal, "
            f"P {reparto.proteinas:.0f} g, G {reparto.grasas:.0f} g, "
            f"C {reparto.carbohidratos:.0f} g\nCandidatos (code | producto | por 100 g):"
        )
        for p in state["candidatos"][reparto.nombre]:
            marca = f" ({p.marca})" if p.marca else ""
            partes.append(
                f"- {p.code} | {p.nombre}{marca} | {p.kcal_100g} kcal, "
                f"P {p.proteinas_100g}, G {p.grasas_100g}, C {p.carbohidratos_100g}"
            )
    if entrada.favoritos:
        partes.append(
            f"\nAlimentos favoritos del usuario (priorízalos): {', '.join(entrada.favoritos)}."
        )
    if entrada.variedad == "sin_repetir":
        partes.append("No repitas el mismo producto en más de una comida.")
    else:
        partes.append("Puedes repetir productos entre comidas si cuadra mejor.")
    feedback = state.get("feedback", "")
    if feedback:
        partes.append(f"\nIMPORTANTE, feedback del intento anterior: {feedback}")
    return "\n".join(partes)


def composer(state: DietState) -> dict:
    salida: PlanComposerLLM = (
        _llm()
        .with_structured_output(PlanComposerLLM, method="json_schema")
        .invoke([("system", COMPOSER_SYSTEM), ("human", _prompt_composer(state))])
    )
    plan = _construir_plan(salida, state["candidatos"])
    return {"plan": plan, "intentos": state.get("intentos", 0) + 1, "feedback": ""}


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
        + ". Ajusta cantidades o cambia productos para corregirlo."
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


def decidir_tras_validar(state: DietState) -> Literal["fin", "composer", "planner"]:
    """Edge condicional tras el Validator."""
    desviacion = state["desviacion"]
    if desviacion is not None and desviacion.dentro_tolerancia:
        return "fin"
    if state.get("aproximado"):
        return "fin"
    if desviacion is not None and abs(desviacion.kcal_pct) > C.UMBRAL_REPLANIFICAR_PCT:
        return "planner"
    return "composer"
