"""Tests del Validator: desviación, mejor intento y routing condicional."""

from __future__ import annotations

from app import constants as C
from app.graph.nodes import (
    _calcular_desviacion,
    _feedback_desviacion,
    _normalizar_reparto,
    _puntuacion,
    decidir_tras_validar,
    validator,
)
from app.schemas import Desviacion, MejorPlan, RepartoComida

from .conftest import hacer_comida


def _plan_exacto(entrada):
    """Un plan que clava el objetivo repartido en 2 comidas."""
    m = entrada.macros_objetivo
    return [
        hacer_comida(
            "Desayuno",
            entrada.kcal_objetivo / 2,
            m.proteinas / 2,
            m.grasas / 2,
            m.carbohidratos / 2,
        ),
        hacer_comida(
            "Cena", entrada.kcal_objetivo / 2, m.proteinas / 2, m.grasas / 2, m.carbohidratos / 2
        ),
    ]


def test_desviacion_plan_exacto_dentro_de_tolerancia(entrada):
    d = _calcular_desviacion(_plan_exacto(entrada), entrada)
    assert d.dentro_tolerancia
    assert d.kcal_pct == 0
    assert d.proteinas_pct == 0


def test_desviacion_kcal_fuera_de_tolerancia(entrada):
    plan = [hacer_comida("Comida", entrada.kcal_objetivo * 1.10, 150, 60, 215)]
    d = _calcular_desviacion(plan, entrada)
    assert not d.dentro_tolerancia
    assert abs(d.kcal_pct - 0.10) < 0.001


def test_desviacion_macro_fuera_de_tolerancia(entrada):
    # kcal clavadas pero proteína un 20% por debajo.
    m = entrada.macros_objetivo
    plan = [
        hacer_comida("Comida", entrada.kcal_objetivo, m.proteinas * 0.8, m.grasas, m.carbohidratos)
    ]
    d = _calcular_desviacion(plan, entrada)
    assert not d.dentro_tolerancia
    assert d.proteinas_pct < -C.TOLERANCIA_MACRO_PCT


def _estado(desviacion: Desviacion, intentos: int = 1, aproximado: bool = False) -> dict:
    return {"desviacion": desviacion, "intentos": intentos, "aproximado": aproximado}


def _desv(kcal=0.0, prot=0.0, grasa=0.0, carbo=0.0, dentro=False) -> Desviacion:
    return Desviacion(
        kcal_pct=kcal,
        proteinas_pct=prot,
        grasas_pct=grasa,
        carbohidratos_pct=carbo,
        dentro_tolerancia=dentro,
    )


def test_routing_dentro_de_tolerancia_termina():
    assert decidir_tras_validar(_estado(_desv(dentro=True))) == "fin"


def test_routing_desvio_moderado_vuelve_a_composer():
    assert decidir_tras_validar(_estado(_desv(kcal=0.08))) == "composer"


def test_routing_desvio_grande_vuelve_a_planner():
    assert decidir_tras_validar(_estado(_desv(kcal=C.UMBRAL_REPLANIFICAR_PCT + 0.05))) == "planner"


def test_routing_aproximado_termina():
    assert decidir_tras_validar(_estado(_desv(kcal=0.5), aproximado=True)) == "fin"


def test_validator_agota_intentos_y_devuelve_el_mejor(entrada):
    plan_malo = [hacer_comida("Comida", entrada.kcal_objetivo * 1.5, 10, 10, 10)]
    mejor_previo = MejorPlan(
        comidas=_plan_exacto(entrada),
        desviacion=_desv(kcal=0.04),
        puntuacion=0.1,
    )
    estado = {
        "entrada": entrada,
        "plan": plan_malo,
        "intentos": C.MAX_INTENTOS,
        "mejor": mejor_previo,
    }
    salida = validator(estado)
    assert salida["aproximado"] is True
    # Devuelve el mejor plan visto, no el último.
    assert salida["plan"] == mejor_previo.comidas
    assert salida["desviacion"] == mejor_previo.desviacion


def test_validator_guarda_feedback_si_quedan_intentos(entrada):
    plan_malo = [hacer_comida("Comida", entrada.kcal_objetivo * 1.10, 150, 60, 215)]
    estado = {"entrada": entrada, "plan": plan_malo, "intentos": 1, "mejor": None}
    salida = validator(estado)
    assert "feedback" in salida and "sobran" in salida["feedback"]
    assert "aproximado" not in salida


def test_puntuacion_pondera_kcal():
    solo_kcal = _puntuacion(_desv(kcal=0.10))
    solo_macro = _puntuacion(_desv(prot=0.10))
    assert solo_kcal == C.PESO_DESVIACION_KCAL * solo_macro


def test_feedback_menciona_solo_lo_desviado(entrada):
    fb = _feedback_desviacion(_desv(kcal=0.10, prot=-0.20), entrada)
    assert "kcal" in fb and "proteína" in fb
    assert "grasa" not in fb


def test_normalizar_reparto_cuadra_totales(entrada):
    comidas = [
        RepartoComida(nombre="Desayuno", kcal=700, proteinas=40, grasas=25, carbohidratos=80),
        RepartoComida(nombre="Comida", kcal=1200, proteinas=70, grasas=40, carbohidratos=120),
        RepartoComida(nombre="Cena", kcal=900, proteinas=50, grasas=30, carbohidratos=90),
    ]
    resultado = _normalizar_reparto(comidas, entrada)
    assert abs(sum(c.kcal for c in resultado) - entrada.kcal_objetivo) < 1
    assert abs(sum(c.proteinas for c in resultado) - entrada.macros_objetivo.proteinas) < 1
    assert abs(sum(c.grasas for c in resultado) - entrada.macros_objetivo.grasas) < 1


def test_normalizar_reparto_degenerado_reparte_uniforme(entrada):
    comidas = [
        RepartoComida(nombre="Desayuno", kcal=0, proteinas=0, grasas=0, carbohidratos=0),
        RepartoComida(nombre="Cena", kcal=0, proteinas=0, grasas=0, carbohidratos=0),
    ]
    resultado = _normalizar_reparto(comidas, entrada)
    assert resultado[0].kcal == entrada.kcal_objetivo / 2
