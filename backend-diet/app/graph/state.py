"""Estado compartido del grafo de generación de dietas."""

from __future__ import annotations

from typing import TypedDict

from ..schemas import Comida, Desviacion, DietInput, MejorPlan, Producto, RepartoComida


class DietState(TypedDict, total=False):
    # input (inmutable durante la ejecución)
    entrada: DietInput

    # trabajo interno
    reparto: list[RepartoComida]  # Planner: objetivo kcal/macros por comida
    candidatos: dict[str, list[Producto]]  # Retriever: candidatos por comida
    plan: list[Comida]  # Composer: comidas con productos y totales reales
    desviacion: Desviacion | None  # Validator
    feedback: str  # feedback explícito para el reintento
    intentos: int
    mejor: MejorPlan | None  # mejor intento visto (para no fallar en silencio)

    # output
    aproximado: bool  # True si se agotaron los intentos fuera de tolerancia
