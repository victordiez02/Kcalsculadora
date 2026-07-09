"""Tests de los helpers deterministas del Chef y del Ajustador."""

from __future__ import annotations

from typing import get_args

import pytest

from app import constants as C
from app.graph.nodes import (
    SinCandidatosError,
    _alinear_platos,
    _construir_comida,
    ajustador,
    decidir_tras_retriever,
)
from app.schemas import (
    CategoriaAlimento,
    IngredienteChef,
    IngredienteResuelto,
    MenuChefLLM,
    PlatoChef,
    PlatoResuelto,
    RepartoComida,
)

from .conftest import hacer_producto


def test_literal_categorias_coincide_con_constants():
    assert set(get_args(CategoriaAlimento)) == set(C.CATEGORIAS)


def hacer_resuelto(ingrediente: str, code: str, **producto_kwargs) -> IngredienteResuelto:
    return IngredienteResuelto(
        ingrediente=IngredienteChef(nombre=ingrediente, categoria="carne", cantidad_g=100),
        producto=hacer_producto(code=code, **producto_kwargs),
    )


def test_construir_comida_calcula_macros_reales():
    plato = PlatoResuelto(
        comida="Comida",
        nombre="Pollo con arroz",
        ingredientes=[
            hacer_resuelto(
                "pechuga de pollo",
                "p1",
                nombre="Pechuga de pollo",
                kcal_100g=105.0,
                proteinas_100g=22.0,
                grasas_100g=2.0,
                carbohidratos_100g=0.0,
            ),
            hacer_resuelto(
                "arroz",
                "a1",
                nombre="Arroz redondo",
                kcal_100g=350.0,
                proteinas_100g=7.0,
                grasas_100g=1.0,
                carbohidratos_100g=77.0,
            ),
        ],
    )
    comida = _construir_comida(plato, [200, 100])
    assert comida.plato == "Pollo con arroz"
    assert comida.kcal == 105.0 * 2 + 350.0  # 560
    assert comida.macros.proteinas == 22.0 * 2 + 7.0
    assert comida.productos[0].ingrediente == "pechuga de pollo"
    assert comida.productos[0].cantidad_g == 200


def test_alinear_platos_rellena_comidas_omitidas():
    salida = MenuChefLLM(platos=[PlatoChef(comida="Cena", nombre="Tortilla", ingredientes=[])])
    platos = _alinear_platos(salida, ["Desayuno", "Comida", "Cena"])
    assert [p.comida for p in platos] == ["Desayuno", "Comida", "Cena"]
    assert platos[0].ingredientes == [] and platos[2].nombre == "Tortilla"


def _estado_platos(n_ingredientes: int, intentos: int) -> dict:
    ingredientes = [hacer_resuelto("pollo", str(i)) for i in range(n_ingredientes)]
    plato = PlatoResuelto(comida="Comida", nombre="Plato", ingredientes=ingredientes)
    return {"platos": [plato], "intentos": intentos}


def test_decidir_tras_retriever_reintenta_chef_si_plato_incompleto():
    estado = _estado_platos(C.INGREDIENTES_POR_PLATO_MIN - 1, intentos=1)
    assert decidir_tras_retriever(estado) == "chef"


def test_decidir_tras_retriever_sigue_si_plato_completo():
    estado = _estado_platos(C.INGREDIENTES_POR_PLATO_MIN, intentos=1)
    assert decidir_tras_retriever(estado) == "ajustador"


def test_decidir_tras_retriever_sigue_sin_intentos_restantes():
    estado = _estado_platos(C.INGREDIENTES_POR_PLATO_MIN - 1, intentos=C.MAX_INTENTOS)
    assert decidir_tras_retriever(estado) == "ajustador"


def test_ajustador_falla_claro_si_plato_sin_ingredientes():
    estado = {
        "reparto": [
            RepartoComida(nombre="Cena", kcal=500, proteinas=40, grasas=15, carbohidratos=50)
        ],
        "platos": [PlatoResuelto(comida="Cena", nombre="", ingredientes=[])],
    }
    with pytest.raises(SinCandidatosError):
        ajustador(estado)
