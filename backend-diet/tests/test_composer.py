"""Tests de los helpers deterministas del Composer."""

from __future__ import annotations

from app import constants as C
from app.graph.nodes import _construir_plan, _redondear_cantidad
from app.schemas import ComidaComposer, ItemComposer, PlanComposerLLM

from .conftest import hacer_producto


def test_redondear_cantidad_clamp_y_multiplos():
    assert _redondear_cantidad(3) == C.CANTIDAD_MIN_G
    assert _redondear_cantidad(9999) == C.CANTIDAD_MAX_G
    assert _redondear_cantidad(123) == 125


def test_construir_plan_calcula_macros_reales():
    pollo = hacer_producto(
        code="p1",
        nombre="Pechuga de pollo",
        categoria="carne",
        kcal_100g=105.0,
        proteinas_100g=22.0,
        grasas_100g=2.0,
        carbohidratos_100g=0.0,
    )
    arroz = hacer_producto(
        code="a1",
        nombre="Arroz",
        categoria="pasta_arroz",
        kcal_100g=350.0,
        proteinas_100g=7.0,
        grasas_100g=1.0,
        carbohidratos_100g=77.0,
    )
    candidatos = {"Comida": [pollo, arroz]}
    salida = PlanComposerLLM(
        comidas=[
            ComidaComposer(
                nombre="Comida",
                items=[
                    ItemComposer(code="p1", cantidad_g=200),
                    ItemComposer(code="a1", cantidad_g=100),
                ],
            )
        ]
    )
    plan = _construir_plan(salida, candidatos)
    assert len(plan) == 1
    comida = plan[0]
    assert comida.kcal == 105.0 * 2 + 350.0  # 560
    assert comida.macros.proteinas == 22.0 * 2 + 7.0
    assert comida.productos[0].cantidad_g == 200


def test_construir_plan_ignora_codigos_alucinados():
    candidatos = {"Cena": [hacer_producto(code="real")]}
    salida = PlanComposerLLM(
        comidas=[
            ComidaComposer(
                nombre="Cena",
                items=[
                    ItemComposer(code="inventado", cantidad_g=100),
                    ItemComposer(code="real", cantidad_g=100),
                ],
            )
        ]
    )
    plan = _construir_plan(salida, candidatos)
    assert [p.nombre for p in plan[0].productos] == ["Producto test"]


def test_construir_plan_comida_sin_salida_del_llm_queda_vacia():
    candidatos = {"Desayuno": [hacer_producto()]}
    plan = _construir_plan(PlanComposerLLM(comidas=[]), candidatos)
    assert plan[0].productos == [] and plan[0].kcal == 0
