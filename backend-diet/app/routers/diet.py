"""Endpoint agregador: genera el plan de comidas completo en una llamada.

Solo orquesta: la lógica vive en app/graph (LangGraph) y app/pdf.
"""

from __future__ import annotations

import base64
import logging

from fastapi import APIRouter, HTTPException

from ..graph.build import get_graph
from ..graph.nodes import SinCandidatosError
from ..pdf.generate import generar_pdf
from ..schemas import DietInput, DietOutput, Macros

router = APIRouter(tags=["diet"])
logger = logging.getLogger(__name__)


@router.post("/diet/generate", response_model=DietOutput)
def generate(payload: DietInput) -> DietOutput:
    """Genera un plan de comidas diario con productos reales del supermercado
    elegido que cuadra con las kcal/macros objetivo, y su PDF descargable."""
    try:
        estado = get_graph().invoke({"entrada": payload, "intentos": 0})
    except SinCandidatosError as e:
        raise HTTPException(status_code=422, detail=str(e)) from e

    plan = estado["plan"]
    kcal_total = round(sum(c.kcal for c in plan), 1)
    macros_totales = Macros(
        proteinas=round(sum(c.macros.proteinas for c in plan), 1),
        grasas=round(sum(c.macros.grasas for c in plan), 1),
        carbohidratos=round(sum(c.macros.carbohidratos for c in plan), 1),
    )
    desviacion = estado["desviacion"]
    intentos_usados = estado.get("intentos", 0)
    aproximado = estado.get("aproximado", False)

    pdf_base64: str | None = None
    try:
        pdf = generar_pdf(
            entrada=payload,
            comidas=plan,
            kcal_total=kcal_total,
            macros_totales=macros_totales,
            desviacion=desviacion,
            intentos_usados=intentos_usados,
            aproximado=aproximado,
        )
        pdf_base64 = base64.b64encode(pdf).decode("ascii")
    except OSError:
        # WeasyPrint sin librerías nativas (p. ej. Windows sin Pango): se
        # devuelve el plan igualmente, solo que sin PDF.
        logger.warning("WeasyPrint no disponible: se devuelve el plan sin PDF.", exc_info=True)

    return DietOutput(
        comidas=plan,
        kcal_total=kcal_total,
        macros_totales=macros_totales,
        desviacion=desviacion,
        intentos_usados=intentos_usados,
        aproximado=aproximado,
        pdf_base64=pdf_base64,
    )
