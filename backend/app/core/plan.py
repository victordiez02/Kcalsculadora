"""Orquestador: combina todos los cálculos en un plan completo."""

from __future__ import annotations

from typing import Literal

from .. import constants as C
from ..schemas import (
    CalculoInput,
    CalculoOutput,
    Detalle,
    Nivel,
    Objetivo,
    Sexo,
)
from .avisos import avisos
from .calorias import calorias_recomendadas
from .ganancia import duracion_minima, ganancia_muscular_semanal
from .get import calcular_get
from .grasa import estimar_grasa, masa_magra
from .imc import calcular_imc, categoria_imc
from .macros import calcular_macros, macros_g_kg
from .peso_objetivo import calcular_peso_objetivo
from .semanas import ajuste_recomendado, calcular_semanas
from .tmb import calcular_tmb


def _ganancia_total_periodo(ganancia_sem: float, semanas: float | None, semanas_min: int) -> float:
    """Pondera la ganancia muscular del periodo siguiendo el Shiny original:
    +30 % por semana sobre el mínimo, o reducción proporcional (hasta 50 %)
    si el plan dura menos."""
    semanas_eff = semanas or semanas_min
    diff = semanas_eff - semanas_min
    if diff > 0:
        return ganancia_sem * (semanas_eff + diff * 0.3)
    return ganancia_sem * semanas_eff * max(0.0, 1 + diff / semanas_min * 0.5)


def _tipo_ajuste(objetivo: Objetivo) -> Literal["reducir", "aumentar", "mantener"]:
    if objetivo == "def":
        return "reducir"
    if objetivo == "vol":
        return "aumentar"
    return "mantener"


def _params_ganancia(
    objetivo: Objetivo, nivel: Nivel, actividad: float, agr_val: float
) -> tuple[float, float, float]:
    """(ganancia_base_mensual, aj_agresividad, aj_actividad)."""
    if objetivo == "vol":
        return (
            C.GANANCIA_MENSUAL_VOL[nivel],
            C.AJUSTE_GANANCIA_VOL_AGRESIVIDAD[agr_val],
            C.ajuste_actividad_vol(actividad),
        )
    if objetivo == "def":
        return (
            C.GANANCIA_MENSUAL_DEF[nivel],
            C.AJUSTE_GANANCIA_DEF_AGRESIVIDAD[agr_val],
            C.ajuste_actividad_def(actividad),
        )
    # mant: curva de definición sin penalización por agresividad
    return (
        C.GANANCIA_MENSUAL_DEF[nivel],
        1.0,
        C.ajuste_actividad_def(actividad),
    )


def _perdida_grasa(
    objetivo: Objetivo,
    sexo: Sexo,
    peso: float,
    peso_obj: float,
    grasa_pct: float,
    enfoque: str,
) -> float:
    if objetivo == "def":
        return max(0.0, peso - peso_obj)
    if objetivo == "mant":
        delta = grasa_pct - C.GRASA_OBJ_MANT[sexo]
        return max(0.0, delta / 100 * peso * C.RECOMP_PERDIDA_GRASA_FACTOR[enfoque])
    return 0.0


def construir_plan(data: CalculoInput) -> CalculoOutput:
    grasa_es_estimada = data.grasa is None
    grasa = (
        estimar_grasa(data.peso, data.altura, data.edad, data.sexo, data.nivel)
        if grasa_es_estimada
        else float(data.grasa)
    )
    enfoque = data.enfoque_recomp or "equilibrio"

    mm = masa_magra(data.peso, grasa)
    tmb = calcular_tmb(data.peso, data.altura, data.edad, data.sexo)
    get_kcal = calcular_get(tmb, data.actividad)
    imc = calcular_imc(data.peso, data.altura)

    ganancia_sem = ganancia_muscular_semanal(
        data.objetivo, data.nivel, data.agresividad, data.actividad, data.sexo, data.edad
    )
    if data.objetivo == "mant":
        ganancia_sem *= C.RECOMP_GANANCIA_FACTOR[enfoque]

    semanas_min = duracion_minima(data.objetivo, data.nivel, grasa, data.sexo)
    peso_obj = calcular_peso_objetivo(
        data.peso, mm, ganancia_sem * semanas_min, data.objetivo, data.sexo, grasa
    )
    calorias = calorias_recomendadas(
        data.objetivo, data.nivel, data.agresividad, get_kcal, enfoque_recomp=data.enfoque_recomp
    )
    macros = calcular_macros(data.peso, peso_obj, calorias, data.objetivo, data.sexo)

    semanas = calcular_semanas(
        data.objetivo, data.agresividad, data.peso, peso_obj, calorias, get_kcal
    )
    if data.objetivo == "mant":
        # En recomposición el balance calórico es ~0: la duración se fija
        # por la mínima recomendada (proceso lento por naturaleza).
        semanas = float(semanas_min)

    ajuste = ajuste_recomendado(
        data.objetivo,
        data.agresividad,
        semanas,
        semanas_min,
        calorias,
        get_kcal,
        data.peso,
        peso_obj,
        data.sexo,
    )
    ganancia_total = _ganancia_total_periodo(ganancia_sem, semanas, semanas_min)
    perdida_grasa_kg = _perdida_grasa(data.objetivo, data.sexo, data.peso, peso_obj, grasa, enfoque)
    grasa_obj = C.GRASA_OBJ_POR_OBJETIVO[data.objetivo][data.sexo]

    detalle = _construir_detalle(data, grasa, ganancia_sem, enfoque)

    return CalculoOutput(
        imc=round(imc, 2),
        imc_categoria=categoria_imc(imc),
        tmb=round(tmb, 1),
        get=round(get_kcal, 1),
        masa_magra=round(mm, 2),
        grasa_estimada=round(grasa, 1),
        grasa_es_estimada=grasa_es_estimada,
        peso_objetivo=round(peso_obj, 2),
        calorias_recomendadas=round(calorias, 0),
        macros=macros,
        semanas_necesarias=round(semanas, 1) if semanas is not None else None,
        semanas_minimas=semanas_min,
        ajuste_calorico=round(abs(calorias - get_kcal), 0),
        tipo_ajuste=_tipo_ajuste(data.objetivo),
        ganancia_muscular_kg=round(max(0.0, ganancia_total), 2),
        perdida_grasa_kg=round(perdida_grasa_kg, 2),
        grasa_objetivo=grasa_obj,
        avisos=avisos(data.objetivo, data.sexo, grasa),
        ajuste_recomendado=ajuste,
        factor_actividad=data.actividad,
        detalle=detalle,
    )


def _construir_detalle(
    data: CalculoInput, grasa: float, ganancia_sem: float, enfoque: str
) -> Detalle:
    """Valores intermedios para la explicación paso a paso del frontend."""
    agr_val = C.AGRESIVIDAD[data.agresividad]
    proteina_g_kg, grasa_g_kg = macros_g_kg(data.objetivo, data.sexo)
    base_mensual, aj_agr, aj_act = _params_ganancia(
        data.objetivo, data.nivel, data.actividad, agr_val
    )

    deficit_rango: list[float] | None = None
    superavit_rango: list[float] | None = None
    superavit_factor: float | None = None
    recomp_pct_get: float | None = None
    recomp_ganancia_factor: float | None = None
    recomp_perdida_grasa_factor: float | None = None
    ajuste_dur: float | None = None

    if data.objetivo == "def":
        deficit_rango = list(C.DEFICIT_POR_NIVEL[data.nivel])
        ajuste_dur = C.AJUSTE_DURACION_DEF[agr_val]
    elif data.objetivo == "vol":
        superavit_rango = list(C.SUPERAVIT_POR_NIVEL[data.nivel])
        superavit_factor = C.AJUSTE_AGRESIVIDAD_VOL_SUPERAVIT[agr_val]
        ajuste_dur = C.AJUSTE_DURACION_VOL[agr_val]
    else:
        recomp_pct_get = C.RECOMP_AJUSTE_PCT_GET[enfoque]
        recomp_ganancia_factor = C.RECOMP_GANANCIA_FACTOR[enfoque]
        recomp_perdida_grasa_factor = C.RECOMP_PERDIDA_GRASA_FACTOR[enfoque]

    return Detalle(
        tmb_coef_peso=C.TMB_PESO,
        tmb_coef_altura=C.TMB_ALTURA,
        tmb_coef_edad=C.TMB_EDAD,
        tmb_offset=C.TMB_OFFSET_HOMBRE if data.sexo == "M" else C.TMB_OFFSET_MUJER,
        grasa_coef_imc=C.GRASA_IMC_COEF,
        grasa_coef_edad=C.GRASA_EDAD_COEF,
        grasa_coef_sexo=C.GRASA_SEXO_COEF,
        grasa_const=C.GRASA_CONST,
        grasa_sexo_val=1 if data.sexo == "M" else 0,
        grasa_edad_efectiva=min(data.edad, C.GRASA_AJUSTE_EDAD_LIMITE),
        grasa_ajuste_nivel=C.GRASA_AJUSTE_NIVEL[data.nivel],
        proteina_g_kg=proteina_g_kg,
        grasa_g_kg=grasa_g_kg,
        kcal_por_g_proteina=C.KCAL_POR_G_PROTEINA,
        kcal_por_g_grasa=C.KCAL_POR_G_GRASA,
        kcal_por_g_carbo=C.KCAL_POR_G_CARBO,
        agresividad_valor=None if data.objetivo == "mant" else agr_val,
        deficit_rango=deficit_rango,
        superavit_rango=superavit_rango,
        superavit_factor=superavit_factor,
        recomp_pct_get=recomp_pct_get,
        recomp_ajuste_max=C.RECOMP_AJUSTE_MAX_KCAL if data.objetivo == "mant" else None,
        recomp_ganancia_factor=recomp_ganancia_factor,
        recomp_perdida_grasa_factor=recomp_perdida_grasa_factor,
        ganancia_base_mensual=base_mensual,
        ganancia_aj_agresividad=aj_agr,
        ganancia_aj_actividad=aj_act,
        ganancia_factor_sexo=(
            C.FACTOR_GANANCIA_SEXO_HOMBRE if data.sexo == "M" else C.FACTOR_GANANCIA_SEXO_MUJER
        ),
        ganancia_factor_edad=C.multiplicador_ganancia_edad(data.edad),
        ganancia_semanal_kg=round(ganancia_sem, 4),
        kcal_por_kg_grasa=C.KCAL_POR_KG_GRASA,
        suavizado_semanas=C.SUAVIZADO_SEMANAS,
        ajuste_duracion=ajuste_dur,
    )
