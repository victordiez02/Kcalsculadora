"""Calorías recomendadas según objetivo, nivel y agresividad."""

from __future__ import annotations

from .. import constants as C
from ..schemas import Agresividad, EnfoqueRecomp, Nivel, Objetivo


def calorias_recomendadas(
    objetivo: Objetivo,
    nivel: Nivel,
    agresividad: Agresividad,
    get_kcal: float,
    enfoque_recomp: EnfoqueRecomp | None = None,
) -> float:
    if objetivo == "mant":
        pct = C.RECOMP_AJUSTE_PCT_GET[enfoque_recomp or "equilibrio"]
        if pct == 0:
            return get_kcal
        ajuste = max(-C.RECOMP_AJUSTE_MAX_KCAL, min(pct * get_kcal, C.RECOMP_AJUSTE_MAX_KCAL))
        return get_kcal + ajuste

    agr_val = C.AGRESIVIDAD[agresividad]
    # Mapeo lineal de agresividad [0.25, 1] -> [lo, hi]
    factor_lineal = (agr_val - 0.25) / 0.75

    if objetivo == "def":
        lo, hi = C.DEFICIT_POR_NIVEL[nivel]
        return get_kcal - (lo + (hi - lo) * factor_lineal)

    # vol — usa una curva ya tabulada en vez del mapeo lineal
    lo, hi = C.SUPERAVIT_POR_NIVEL[nivel]
    factor = C.AJUSTE_AGRESIVIDAD_VOL_SUPERAVIT[agr_val]
    return get_kcal + lo + (hi - lo) * factor
