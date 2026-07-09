"""Tests del renderizado de la plantilla del PDF (sin WeasyPrint)."""

from __future__ import annotations

from app.pdf.generate import renderizar_html
from app.schemas import Desviacion, Macros, MenuDia, Semana

from .conftest import hacer_comida


def _desviacion(dentro: bool = True) -> Desviacion:
    return Desviacion(
        kcal_pct=0.01,
        proteinas_pct=-0.02,
        grasas_pct=0.0,
        carbohidratos_pct=0.03,
        dentro_tolerancia=dentro,
    )


def _menu(dias: list[str], aproximado: bool = False) -> MenuDia:
    comidas = [
        hacer_comida("Desayuno", 500, 35, 15, 55),
        hacer_comida("Comida", 800, 60, 25, 85),
        hacer_comida("Cena", 700, 55, 20, 75),
    ]
    return MenuDia(
        dias=dias,
        comidas=comidas,
        kcal=2000.0,
        macros=Macros(proteinas=150, grasas=60, carbohidratos=215),
        desviacion=_desviacion(),
        aproximado=aproximado,
    )


def _semana(aproximado: bool = False) -> Semana:
    return Semana(
        menus=[
            _menu(["Lunes", "Miércoles", "Viernes", "Domingo"], aproximado=aproximado),
            _menu(["Martes", "Jueves", "Sábado"]),
        ],
        kcal_diarias=2000.0,
        macros_diarios=Macros(proteinas=150, grasas=60, carbohidratos=215),
        intentos_usados=2,
        aproximado=aproximado,
    )


def test_renderiza_datos_del_plan(entrada):
    html = renderizar_html(entrada=entrada, semana=_semana())
    assert "Plan semanal" in html
    assert "Mercadona" in html
    assert "Lunes · Miércoles · Viernes · Domingo" in html
    assert "Martes · Jueves · Sábado" in html
    assert "Desayuno" in html and "Cena" in html
    assert "2000 kcal" in html
    assert "Dentro de objetivo" in html
    assert "Plan aproximado" not in html


def test_renderiza_badge_aproximado(entrada):
    html = renderizar_html(entrada=entrada, semana=_semana(aproximado=True))
    assert "Plan aproximado" in html
    assert "mejor encontrado" in html


def test_escapa_html_en_nombres(entrada):
    semana = _semana()
    semana.menus[0].comidas[0].productos[0].nombre = "<script>alert(1)</script>"
    html = renderizar_html(entrada=entrada, semana=semana)
    assert "<script>alert(1)" not in html
