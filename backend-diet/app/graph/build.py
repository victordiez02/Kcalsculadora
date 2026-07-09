"""Construcción del StateGraph y sus edges condicionales.

Flujo: Planner -> Chef -> Retriever -> Ajustador -> Validator
  - Retriever: si un plato se queda sin ingredientes disponibles -> Chef
    (nuevo plato, evitando los no disponibles)
  - Validator dentro de tolerancia      -> fin
  - Validator con desvío moderado       -> Chef (otros platos, con feedback)
  - Validator con desvío grande de kcal -> Planner (nuevo reparto)
  - intentos agotados                   -> fin con el mejor plan, marcado aproximado
"""

from __future__ import annotations

from functools import lru_cache

from langgraph.graph import END, START, StateGraph

from .nodes import (
    ajustador,
    chef,
    decidir_inicio,
    decidir_tras_retriever,
    decidir_tras_validar,
    planner,
    retriever,
    validator,
)
from .state import DietState


@lru_cache(maxsize=1)
def get_graph():
    g = StateGraph(DietState)
    g.add_node("planner", planner)
    g.add_node("chef", chef)
    g.add_node("retriever", retriever)
    g.add_node("ajustador", ajustador)
    g.add_node("validator", validator)

    # Con un reparto ya calculado (menús 2..n de la semana) se entra por el Chef.
    g.add_conditional_edges(START, decidir_inicio, {"planner": "planner", "chef": "chef"})
    g.add_edge("planner", "chef")
    g.add_edge("chef", "retriever")
    g.add_conditional_edges(
        "retriever",
        decidir_tras_retriever,
        {"ajustador": "ajustador", "chef": "chef"},
    )
    g.add_edge("ajustador", "validator")
    g.add_conditional_edges(
        "validator",
        decidir_tras_validar,
        {"fin": END, "chef": "chef", "planner": "planner"},
    )
    return g.compile()
