"""Tests del renderizado de la plantilla del PDF (sin WeasyPrint)."""

from __future__ import annotations

from app.pdf.generate import renderizar_html
from app.schemas import Desviacion, Macros

from .conftest import hacer_comida


def _kwargs(entrada, aproximado=False):
    comidas = [
        hacer_comida("Desayuno", 500, 35, 15, 55),
        hacer_comida("Comida", 800, 60, 25, 85),
        hacer_comida("Cena", 700, 55, 20, 75),
    ]
    return {
        "entrada": entrada,
        "comidas": comidas,
        "kcal_total": 2000.0,
        "macros_totales": Macros(proteinas=150, grasas=60, carbohidratos=215),
        "desviacion": Desviacion(
            kcal_pct=0.01,
            proteinas_pct=-0.02,
            grasas_pct=0.0,
            carbohidratos_pct=0.05,
            dentro_tolerancia=True,
        ),
        "intentos_usados": 2,
        "aproximado": aproximado,
    }


def test_renderiza_datos_del_plan(entrada):
    html = renderizar_html(**_kwargs(entrada))
    assert "Plan de comidas" in html
    assert "Mercadona" in html
    assert "Desayuno" in html and "Cena" in html
    assert "2000 kcal" in html
    assert "Dentro de objetivo" in html
    assert "Plan aproximado" not in html


def test_renderiza_badge_aproximado(entrada):
    html = renderizar_html(**_kwargs(entrada, aproximado=True))
    assert "Plan aproximado" in html
    assert "mejor plan encontrado" in html


def test_escapa_html_en_nombres(entrada):
    kwargs = _kwargs(entrada)
    kwargs["comidas"][0].productos[0].nombre = "<script>alert(1)</script>"
    html = renderizar_html(**kwargs)
    assert "<script>alert(1)" not in html
