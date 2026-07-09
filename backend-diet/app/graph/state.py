"""Estado compartido del grafo de generación de dietas."""

from __future__ import annotations

from typing import TypedDict

from ..schemas import (
    Comida,
    Desviacion,
    DietInput,
    MejorPlan,
    PlatoChef,
    PlatoResuelto,
    RepartoComida,
)


class DietState(TypedDict, total=False):
    # input (inmutable durante la ejecución)
    entrada: DietInput

    # trabajo interno
    reparto: list[RepartoComida]  # Planner: objetivo kcal/macros por comida
    platos_usados: list[str]  # platos de menús anteriores de la semana (no repetir)
    platos_chef: list[PlatoChef]  # Chef: un plato por comida, ingredientes genéricos
    platos: list[PlatoResuelto]  # Retriever: ingredientes -> productos reales
    no_encontrados: list[str]  # ingredientes sin producto en el súper (acumulado)
    plan: list[Comida]  # Ajustador: comidas con gramos y macros reales
    desviacion: Desviacion | None  # Validator
    feedback: str  # feedback explícito para el reintento
    intentos: int
    mejor: MejorPlan | None  # mejor intento visto (para no fallar en silencio)

    # output
    aproximado: bool  # True si se agotaron los intentos fuera de tolerancia
