"""Endpoint agregador: ejecuta el plan completo en una sola llamada."""

from __future__ import annotations

from fastapi import APIRouter

from ..core.plan import construir_plan
from ..schemas import CalculoInput, CalculoOutput

router = APIRouter(tags=["plan"])


@router.post("/plan", response_model=CalculoOutput)
def plan(payload: CalculoInput) -> CalculoOutput:
    """Devuelve el plan nutricional completo (TMB, GET, macros, peso objetivo,
    semanas, avisos y ajuste recomendado) a partir de los datos del usuario."""
    return construir_plan(payload)
