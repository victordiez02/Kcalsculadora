"""Renderiza la plantilla Jinja2 fija y la convierte a PDF con WeasyPrint.

La estética vive en template.html (versionada, SIEMPRE la misma); aquí solo
se construye el contexto y se renderiza. WeasyPrint se importa en diferido
porque en Windows requiere las librerías nativas de Pango (en Docker van
incluidas); si no están disponibles, el endpoint devuelve el plan sin PDF.
"""

from __future__ import annotations

from datetime import date
from pathlib import Path

from jinja2 import Environment, FileSystemLoader, StrictUndefined

from .. import constants as C
from ..schemas import Comida, Desviacion, DietInput, Macros

_env = Environment(
    loader=FileSystemLoader(Path(__file__).parent),
    autoescape=True,
    undefined=StrictUndefined,
)

MESES = [
    "enero",
    "febrero",
    "marzo",
    "abril",
    "mayo",
    "junio",
    "julio",
    "agosto",
    "septiembre",
    "octubre",
    "noviembre",
    "diciembre",
]

OBJETIVO_LABELS = {"def": "Definición", "vol": "Volumen", "mant": "Recomposición"}


def _contexto(
    entrada: DietInput,
    comidas: list[Comida],
    kcal_total: float,
    macros_totales: Macros,
    desviacion: Desviacion,
    intentos_usados: int,
    aproximado: bool,
) -> dict:
    hoy = date.today()
    return {
        "fecha": f"{hoy.day} de {MESES[hoy.month - 1]} de {hoy.year}",
        "supermercado": C.SUPERMERCADOS[entrada.supermercado]["label"],
        "objetivo": OBJETIVO_LABELS[entrada.objetivo],
        "kcal_objetivo": entrada.kcal_objetivo,
        "macros_objetivo": entrada.macros_objetivo,
        "kcal_total": kcal_total,
        "macros_totales": macros_totales,
        "comidas": comidas,
        "desviacion": desviacion,
        "aproximado": aproximado,
        "intentos_usados": intentos_usados,
        "tolerancia_kcal": C.TOLERANCIA_KCAL_PCT,
        "tolerancia_macro": C.TOLERANCIA_MACRO_PCT,
    }


def renderizar_html(**kwargs) -> str:
    """Renderiza la plantilla a HTML (separado para poder testearlo sin
    las librerías nativas de WeasyPrint)."""
    return _env.get_template("template.html").render(**_contexto(**kwargs))


def generar_pdf(**kwargs) -> bytes:
    from weasyprint import HTML  # import diferido: requiere Pango

    return HTML(string=renderizar_html(**kwargs)).write_pdf()
