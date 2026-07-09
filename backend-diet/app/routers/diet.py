"""Endpoint agregador: genera el plan semanal completo en una llamada.

Solo orquesta: la lógica vive en app/semana.py (que invoca el grafo de
app/graph) y en app/pdf.
"""

from __future__ import annotations

import base64
import logging

from fastapi import APIRouter, HTTPException

from ..graph.nodes import SinCandidatosError
from ..pdf.generate import generar_pdf
from ..schemas import DietInput, DietOutput
from ..semana import generar_semana

router = APIRouter(tags=["diet"])
logger = logging.getLogger(__name__)


@router.post("/diet/generate", response_model=DietOutput)
def generate(payload: DietInput) -> DietOutput:
    """Genera un plan semanal de comidas con productos reales del supermercado
    elegido que cuadra con las kcal/macros objetivo, y su PDF descargable.
    El plan completo va en el PDF; la respuesta lleva el resumen diario y el
    menú del lunes como ejemplo."""
    try:
        semana = generar_semana(payload)
    except SinCandidatosError as e:
        raise HTTPException(status_code=422, detail=str(e)) from e

    pdf_base64: str | None = None
    try:
        pdf = generar_pdf(entrada=payload, semana=semana)
        pdf_base64 = base64.b64encode(pdf).decode("ascii")
    except OSError:
        # WeasyPrint sin librerías nativas (p. ej. Windows sin Pango): se
        # devuelve el plan igualmente, solo que sin PDF.
        logger.warning("WeasyPrint no disponible: se devuelve el plan sin PDF.", exc_info=True)

    return DietOutput(
        kcal_diarias=semana.kcal_diarias,
        macros_diarios=semana.macros_diarios,
        menu_ejemplo=semana.menus[0],
        menus_distintos=len(semana.menus),
        intentos_usados=semana.intentos_usados,
        aproximado=semana.aproximado,
        pdf_base64=pdf_base64,
    )
