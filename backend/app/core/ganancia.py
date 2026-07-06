"""Ganancia muscular esperada y duración mínima recomendada."""

from __future__ import annotations

from .. import constants as C
from ..schemas import Agresividad, Nivel, Objetivo, Sexo


def ganancia_muscular_semanal(
    objetivo: Objetivo,
    nivel: Nivel,
    agresividad: Agresividad,
    actividad: float,
    sexo: Sexo,
    edad: int,
) -> float:
    agr_val = C.AGRESIVIDAD[agresividad]
    if objetivo == "vol":
        base = C.GANANCIA_MENSUAL_VOL[nivel]
        aj_agr = C.AJUSTE_GANANCIA_VOL_AGRESIVIDAD[agr_val]
        aj_act = C.ajuste_actividad_vol(actividad)
    elif objetivo == "def":
        base = C.GANANCIA_MENSUAL_DEF[nivel]
        aj_agr = C.AJUSTE_GANANCIA_DEF_AGRESIVIDAD[agr_val]
        aj_act = C.ajuste_actividad_def(actividad)
    else:
        # Recomposición: usa la curva de definición (crecimiento en balance
        # casi neutro), sin penalización por agresividad (oculta en UI).
        base = C.GANANCIA_MENSUAL_DEF[nivel]
        aj_agr = 1.0
        aj_act = C.ajuste_actividad_def(actividad)

    semanal = (base / 4) * aj_agr * aj_act
    factor_sexo = C.FACTOR_GANANCIA_SEXO_HOMBRE if sexo == "M" else C.FACTOR_GANANCIA_SEXO_MUJER
    return semanal * factor_sexo * C.multiplicador_ganancia_edad(edad)


def duracion_minima(objetivo: Objetivo, nivel: Nivel, grasa_pct: float, sexo: Sexo) -> int:
    if objetivo == "mant":
        return C.DURACION_MIN_RECOMP
    if objetivo == "def":
        return C.DURACION_MIN_DEF[nivel]
    base = C.DURACION_MIN_VOL[nivel]
    if (sexo == "M" and grasa_pct < C.GRASA_BAJA_VOL["M"]) or (
        sexo == "F" and grasa_pct < C.GRASA_BAJA_VOL["F"]
    ):
        base += C.EXTENSION_VOL_GRASA_BAJA_SEMANAS
    return base
