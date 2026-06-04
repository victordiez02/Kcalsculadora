"""Estimación de % grasa corporal (Deurenberg + variante >36 años)."""

from __future__ import annotations

from .. import constants as C
from ..schemas import Nivel, Sexo
from .imc import calcular_imc


def estimar_grasa(
    peso: float,
    altura_cm: float,
    edad: int,
    sexo: Sexo,
    nivel: Nivel = "principiante",
) -> float:
    imc = calcular_imc(peso, altura_cm)
    sexo_val = 1 if sexo == "M" else 0
    # Clamp de edad: se evita que el término crezca sin límite en personas mayores
    edad_efectiva = min(edad, C.GRASA_AJUSTE_EDAD_LIMITE)
    grasa = (
        C.GRASA_IMC_COEF * imc
        + C.GRASA_EDAD_COEF * edad_efectiva
        - C.GRASA_SEXO_COEF * sexo_val
        - C.GRASA_CONST
    )
    # Se ajusta por nivel de entrenamiento
    grasa -= C.GRASA_AJUSTE_NIVEL[nivel]
    return round(grasa, 1)


def masa_magra(peso: float, grasa_pct: float) -> float:
    return peso * (1 - grasa_pct / 100)
