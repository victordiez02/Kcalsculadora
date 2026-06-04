"""Tasa Metabólica Basal — fórmula de Mifflin-St Jeor."""

from __future__ import annotations

from .. import constants as C
from ..schemas import Sexo


def calcular_tmb(peso: float, altura_cm: float, edad: int, sexo: Sexo) -> float:
    offset = C.TMB_OFFSET_HOMBRE if sexo == "M" else C.TMB_OFFSET_MUJER
    return C.TMB_PESO * peso + C.TMB_ALTURA * altura_cm - C.TMB_EDAD * edad + offset
