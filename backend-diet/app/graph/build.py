"""Construcción del StateGraph y sus edges condicionales.

Flujo: Planner -> Retriever -> Composer -> Validator
  - dentro de tolerancia            -> fin
  - desvío moderado                 -> Composer (reintento con feedback)
  - desvío grande de kcal           -> Planner (nuevo reparto con feedback)
  - intentos agotados               -> fin con el mejor plan, marcado aproximado
"""

from __future__ import annotations

from functools import lru_cache

from langgraph.graph import END, START, StateGraph

from .nodes import composer, decidir_tras_validar, planner, retriever, validator
from .state import DietState


@lru_cache(maxsize=1)
def get_graph():
    g = StateGraph(DietState)
    g.add_node("planner", planner)
    g.add_node("retriever", retriever)
    g.add_node("composer", composer)
    g.add_node("validator", validator)

    g.add_edge(START, "planner")
    g.add_edge("planner", "retriever")
    g.add_edge("retriever", "composer")
    g.add_edge("composer", "validator")
    g.add_conditional_edges(
        "validator",
        decidir_tras_validar,
        {"fin": END, "composer": "composer", "planner": "planner"},
    )
    return g.compile()
