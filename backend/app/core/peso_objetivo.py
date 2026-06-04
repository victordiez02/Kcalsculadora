"""Cálculo del peso objetivo."""

from __future__ import annotations

from .. import constants as C
from ..schemas import Objetivo, Sexo


def calcular_peso_objetivo(
    peso: float,
    masa_magra: float,
    incremento_masa_magra: float,
    objetivo: Objetivo,
    sexo: Sexo,
    grasa_pct: float,
) -> float:
    # Si el % graso está fuera del rango razonable para el objetivo,
    # se devuelve el peso actual sin cambios.
    if objetivo == "def" and grasa_pct < C.GRASA_DEF_LIMITE_ADVERTENCIA[sexo]:
        return peso
    if objetivo == "vol" and grasa_pct > C.GRASA_OBJ_VOL[sexo]:
        return peso

    grasa_meta = C.GRASA_OBJ_POR_OBJETIVO[objetivo][sexo] / 100
    return (masa_magra + incremento_masa_magra) / (1 - grasa_meta)
