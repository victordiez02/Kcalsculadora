"""Ajuste determinista de las cantidades de un plato.

El LLM decide QUÉ se come (el plato y sus ingredientes); aquí se decide
CUÁNTO. Los gramos de cada ingrediente se resuelven como un problema de
mínimos cuadrados con cotas (scipy.optimize.lsq_linear): minimizar el error
relativo frente al objetivo de kcal/macros de la comida, manteniendo cada
cantidad cerca de la orientativa que propuso el Chef para que el plato siga
siendo realista. Esto elimina el error aritmético de dejar que el LLM
calcule cantidades.
"""

from __future__ import annotations

import numpy as np
from scipy.optimize import lsq_linear

from .. import constants as C
from ..schemas import Producto, RepartoComida


def _cotas(orientativa_g: float) -> tuple[float, float]:
    """Cotas [min, max] en gramos para un ingrediente, alrededor de su
    cantidad orientativa y dentro del rango global. Garantiza min < max."""
    lo = min(
        max(orientativa_g * C.AJUSTE_FACTOR_MIN, C.CANTIDAD_MIN_G),
        C.CANTIDAD_MAX_G - C.REDONDEO_CANTIDAD_G,
    )
    hi = max(min(orientativa_g * C.AJUSTE_FACTOR_MAX, C.CANTIDAD_MAX_G), lo + C.REDONDEO_CANTIDAD_G)
    return lo, hi


def _redondear(g: float) -> float:
    g = max(C.CANTIDAD_MIN_G, min(C.CANTIDAD_MAX_G, g))
    return round(g / C.REDONDEO_CANTIDAD_G) * C.REDONDEO_CANTIDAD_G


def ajustar_cantidades(
    productos: list[Producto],
    orientativas_g: list[float],
    objetivo: RepartoComida,
) -> list[float]:
    """Gramos finales de cada producto para acercar la comida a su objetivo.

    Minimiza el error RELATIVO (cada fila se normaliza por su objetivo, y la
    de kcal pesa más, igual que en la puntuación del Validator), con cada
    cantidad acotada alrededor de la orientativa del Chef y redondeada a
    múltiplos de REDONDEO_CANTIDAD_G.
    """
    if not productos:
        return []

    # A[fila][col] = nutriente por gramo del producto col (kcal, P, G, C).
    matriz = np.array(
        [
            [p.kcal_100g / 100 for p in productos],
            [p.proteinas_100g / 100 for p in productos],
            [p.grasas_100g / 100 for p in productos],
            [p.carbohidratos_100g / 100 for p in productos],
        ]
    )
    objetivos = np.array(
        [objetivo.kcal, objetivo.proteinas, objetivo.grasas, objetivo.carbohidratos]
    )
    pesos = np.array([C.PESO_DESVIACION_KCAL, 1.0, 1.0, 1.0]) / np.maximum(
        objetivos, C.AJUSTE_DIVISOR_MINIMO
    )

    cotas = [_cotas(g) for g in orientativas_g]
    resultado = lsq_linear(
        matriz * pesos[:, None],
        objetivos * pesos,
        bounds=([lo for lo, _ in cotas], [hi for _, hi in cotas]),
    )
    return [_redondear(g) for g in resultado.x]
