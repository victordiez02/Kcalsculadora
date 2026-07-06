"""Semanas necesarias para alcanzar el peso objetivo y ajuste suavizado
si el plan resulta más corto que el mínimo recomendado."""

from __future__ import annotations

from .. import constants as C
from ..schemas import (
    Agresividad,
    AjusteRecomendado,
    Objetivo,
    Sexo,
)
from .macros import calcular_macros


def calcular_semanas(
    objetivo: Objetivo,
    agresividad: Agresividad,
    peso: float,
    peso_objetivo: float,
    calorias_recomendadas: float,
    get_kcal: float,
) -> float | None:
    if objetivo == "mant":
        return None
    delta_kcal = abs(get_kcal - calorias_recomendadas)
    if delta_kcal == 0:
        return None

    agr_val = C.AGRESIVIDAD[agresividad]
    cambio_peso = abs(peso_objetivo - peso)
    kcal_totales = cambio_peso * C.KCAL_POR_KG_GRASA
    ajuste_dur = (
        C.AJUSTE_DURACION_DEF[agr_val] if objetivo == "def" else C.AJUSTE_DURACION_VOL[agr_val]
    )
    return (kcal_totales / delta_kcal / (7 * C.SUAVIZADO_SEMANAS)) * ajuste_dur


def ajuste_recomendado(
    objetivo: Objetivo,
    agresividad: Agresividad,
    semanas: float | None,
    semanas_min: int,
    calorias_recomendadas: float,
    get_kcal: float,
    peso: float,
    peso_objetivo: float,
    sexo: Sexo,
) -> AjusteRecomendado | None:
    if objetivo == "mant" or semanas is None or semanas >= semanas_min:
        return None

    agr_val = C.AGRESIVIDAD[agresividad]
    diferencia = semanas_min - semanas
    bonus = (
        min(max(diferencia - 1, 0), C.AJUSTE_KCAL_MAX_SEMANAS_EXTRA)
        * C.AJUSTE_KCAL_POR_SEMANA_EXTRA
    )
    ajuste_dia = C.AJUSTE_KCAL_BASE_DIA + bonus + C.AJUSTE_KCAL_POR_AGRESIVIDAD[agr_val]

    if objetivo == "def":
        nuevas_kcal = calorias_recomendadas + ajuste_dia
        motivo = "Definición demasiado agresiva: se suaviza el déficit."
    else:
        nuevas_kcal = calorias_recomendadas - ajuste_dia
        motivo = "Volumen demasiado agresivo: se suaviza el superávit."

    nuevos_macros = calcular_macros(peso, peso_objetivo, nuevas_kcal, objetivo, sexo)
    g_semana = abs(peso - peso_objetivo) / semanas_min * 1000 if semanas_min > 0 else 0
    return AjusteRecomendado(
        calorias=round(nuevas_kcal, 0),
        macros=nuevos_macros,
        semanas_minimas=semanas_min,
        delta_kcal_dia=round(abs(nuevas_kcal - get_kcal), 0),
        gramos_por_semana=round(g_semana, 0),
        motivo=motivo,
    )
