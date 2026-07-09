"""Tests del ajustador determinista de cantidades (sin LLM)."""

from __future__ import annotations

from app import constants as C
from app.graph.cantidades import ajustar_cantidades
from app.schemas import RepartoComida

from .conftest import hacer_producto

POLLO = hacer_producto(
    code="p", nombre="Pollo", kcal_100g=105, proteinas_100g=22, grasas_100g=2, carbohidratos_100g=0
)
ARROZ = hacer_producto(
    code="a", nombre="Arroz", kcal_100g=350, proteinas_100g=7, grasas_100g=1, carbohidratos_100g=77
)
ACEITE = hacer_producto(
    code="o",
    nombre="Aceite",
    kcal_100g=900,
    proteinas_100g=0,
    grasas_100g=100,
    carbohidratos_100g=0,
)


def _totales(productos, gramos):
    pares = list(zip(productos, gramos, strict=True))
    kcal = sum(p.kcal_100g * g / 100 for p, g in pares)
    prot = sum(p.proteinas_100g * g / 100 for p, g in pares)
    grasa = sum(p.grasas_100g * g / 100 for p, g in pares)
    carbo = sum(p.carbohidratos_100g * g / 100 for p, g in pares)
    return kcal, prot, grasa, carbo


def test_ajuste_cuadra_un_objetivo_alcanzable():
    productos = [POLLO, ARROZ, ACEITE]
    objetivo = RepartoComida(nombre="Comida", kcal=700, proteinas=50, grasas=20, carbohidratos=77)
    gramos = ajustar_cantidades(productos, [150, 90, 10], objetivo)

    kcal, prot, grasa, carbo = _totales(productos, gramos)
    assert abs(kcal - objetivo.kcal) / objetivo.kcal <= C.TOLERANCIA_KCAL_PCT
    assert abs(prot - objetivo.proteinas) / objetivo.proteinas <= C.TOLERANCIA_MACRO_PCT
    assert abs(grasa - objetivo.grasas) / objetivo.grasas <= C.TOLERANCIA_MACRO_PCT
    assert abs(carbo - objetivo.carbohidratos) / objetivo.carbohidratos <= C.TOLERANCIA_MACRO_PCT


def test_ajuste_respeta_cotas_alrededor_de_la_orientativa():
    # Objetivo absurdo de grasa: el aceite no puede pasar de orientativa * factor.
    objetivo = RepartoComida(nombre="Cena", kcal=2000, proteinas=0, grasas=220, carbohidratos=0)
    (gramos,) = ajustar_cantidades([ACEITE], [20], objetivo)
    assert gramos <= 20 * C.AJUSTE_FACTOR_MAX
    assert gramos >= C.CANTIDAD_MIN_G


def test_ajuste_redondea_a_multiplos():
    objetivo = RepartoComida(nombre="Comida", kcal=500, proteinas=40, grasas=15, carbohidratos=50)
    gramos = ajustar_cantidades([POLLO, ARROZ], [150, 100], objetivo)
    assert all(g % C.REDONDEO_CANTIDAD_G == 0 for g in gramos)
    assert all(C.CANTIDAD_MIN_G <= g <= C.CANTIDAD_MAX_G for g in gramos)


def test_ajuste_sin_productos_devuelve_vacio():
    objetivo = RepartoComida(nombre="Comida", kcal=500, proteinas=40, grasas=15, carbohidratos=50)
    assert ajustar_cantidades([], [], objetivo) == []


def test_ajuste_no_explota_con_objetivo_de_macro_cero():
    objetivo = RepartoComida(nombre="Merienda", kcal=200, proteinas=15, grasas=0, carbohidratos=25)
    gramos = ajustar_cantidades([POLLO, ARROZ], [80, 40], objetivo)
    assert len(gramos) == 2
