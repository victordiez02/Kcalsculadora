"""IMC y categorización."""

from __future__ import annotations

from .. import constants as C


def calcular_imc(peso: float, altura_cm: float) -> float:
    return peso / (altura_cm / 100) ** 2


def categoria_imc(imc: float) -> str:
    for tope, nombre in C.IMC_CATEGORIAS:
        if imc < tope:
            return nombre
    return C.IMC_CATEGORIAS[-1][1]
