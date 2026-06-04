"""Distribución de macronutrientes."""

from __future__ import annotations

from .. import constants as C
from ..schemas import Macros, Objetivo, Sexo

_PROTEINA_G_KG = {
    "def": C.PROTEINA_DEF_G_KG,
    "vol": C.PROTEINA_VOL_G_KG,
    "mant": C.PROTEINA_MANT_G_KG,
}
_GRASA_G_KG = {
    "def": C.GRASA_DEF_G_KG,
    "vol": C.GRASA_VOL_G_KG,
    "mant": C.GRASA_MANT_G_KG,
}


def macros_g_kg(objetivo: Objetivo, sexo: Sexo) -> tuple[float, float]:
    """Devuelve (proteína g/kg, grasa g/kg) para el objetivo y sexo dados."""
    return _PROTEINA_G_KG[objetivo][sexo], _GRASA_G_KG[objetivo][sexo]


def calcular_macros(
    peso: float,
    peso_objetivo: float,
    calorias: float,
    objetivo: Objetivo,
    sexo: Sexo,
) -> Macros:
    del peso_objetivo  # reservado por compatibilidad de API (no participa hoy)
    prot_kg, gra_kg = macros_g_kg(objetivo, sexo)
    proteinas = peso * prot_kg
    grasas = peso * gra_kg
    kcal_prot = proteinas * C.KCAL_POR_G_PROTEINA
    kcal_gra = grasas * C.KCAL_POR_G_GRASA
    carbohidratos = max(0.0, (calorias - kcal_prot - kcal_gra) / C.KCAL_POR_G_CARBO)
    return Macros(
        proteinas=round(proteinas, 1),
        grasas=round(grasas, 1),
        carbohidratos=round(carbohidratos, 1),
    )
