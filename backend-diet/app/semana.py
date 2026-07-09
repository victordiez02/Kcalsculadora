"""Generación del plan semanal a partir del grafo de un día.

El grafo genera UN menú diario; aquí se invoca tantas veces como menús
distintos pida la variedad elegida y se reparten en ciclo sobre los 7 días
(día d -> menú d % n). Entre menús se propagan el reparto del Planner (solo
depende de la entrada), los platos ya usados (para no repetirlos) y los
ingredientes que no existen en el súper (para no volver a proponerlos).
"""

from __future__ import annotations

from . import constants as C
from .graph.build import get_graph
from .schemas import Comida, DietInput, Macros, MenuDia, Semana


def _totales(comidas: list[Comida]) -> tuple[float, Macros]:
    kcal = round(sum(c.kcal for c in comidas), 1)
    macros = Macros(
        proteinas=round(sum(c.macros.proteinas for c in comidas), 1),
        grasas=round(sum(c.macros.grasas for c in comidas), 1),
        carbohidratos=round(sum(c.macros.carbohidratos for c in comidas), 1),
    )
    return kcal, macros


def _dias_por_menu(n_menus: int) -> list[list[str]]:
    dias: list[list[str]] = [[] for _ in range(n_menus)]
    for d, nombre in enumerate(C.DIAS_SEMANA):
        dias[d % n_menus].append(nombre)
    return dias


def generar_semana(entrada: DietInput) -> Semana:
    n_menus = C.VARIEDADES[entrada.variedad]["menus"]
    grafo = get_graph()
    dias = _dias_por_menu(n_menus)

    menus: list[MenuDia] = []
    reparto = None
    platos_usados: list[str] = []
    no_encontrados: list[str] = []
    intentos_usados = 0
    for i in range(n_menus):
        inicial: dict = {
            "entrada": entrada,
            "intentos": 0,
            "platos_usados": platos_usados,
            "no_encontrados": no_encontrados,
        }
        if reparto is not None:
            inicial["reparto"] = reparto
        estado = grafo.invoke(inicial)

        reparto = estado["reparto"]
        no_encontrados = estado.get("no_encontrados", [])
        platos_usados = platos_usados + [c.plato for c in estado["plan"] if c.plato]
        intentos_usados += estado.get("intentos", 0)

        kcal, macros = _totales(estado["plan"])
        menus.append(
            MenuDia(
                dias=dias[i],
                comidas=estado["plan"],
                kcal=kcal,
                macros=macros,
                desviacion=estado["desviacion"],
                aproximado=estado.get("aproximado", False),
            )
        )

    dias_semana = len(C.DIAS_SEMANA)
    return Semana(
        menus=menus,
        kcal_diarias=round(sum(m.kcal * len(m.dias) for m in menus) / dias_semana, 1),
        macros_diarios=Macros(
            proteinas=round(sum(m.macros.proteinas * len(m.dias) for m in menus) / dias_semana, 1),
            grasas=round(sum(m.macros.grasas * len(m.dias) for m in menus) / dias_semana, 1),
            carbohidratos=round(
                sum(m.macros.carbohidratos * len(m.dias) for m in menus) / dias_semana, 1
            ),
        ),
        intentos_usados=intentos_usados,
        aproximado=any(m.aproximado for m in menus),
    )
