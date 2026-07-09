"""Tests de los helpers deterministas del plan semanal (sin grafo ni LLM)."""

from __future__ import annotations

from app import constants as C
from app.semana import _dias_por_menu, _totales

from .conftest import hacer_comida


def test_dias_por_menu_cubre_los_siete_dias():
    for variedad in C.VARIEDADES.values():
        dias = _dias_por_menu(variedad["menus"])
        assert sum(len(d) for d in dias) == 7
        assert all(len(d) >= 1 for d in dias)


def test_dias_por_menu_alterna_en_ciclo():
    assert _dias_por_menu(2) == [
        ["Lunes", "Miércoles", "Viernes", "Domingo"],
        ["Martes", "Jueves", "Sábado"],
    ]
    # El primer menú siempre es el del lunes (es el ejemplo de la app).
    assert _dias_por_menu(7)[0] == ["Lunes"]


def test_totales_suma_las_comidas():
    comidas = [
        hacer_comida("Desayuno", 500, 35, 15, 55),
        hacer_comida("Cena", 700, 55, 20, 75),
    ]
    kcal, macros = _totales(comidas)
    assert kcal == 1200
    assert macros.proteinas == 90
    assert macros.grasas == 35
    assert macros.carbohidratos == 130
