"""Constantes y parámetros de las fórmulas.

Toda la configuración numérica (multiplicadores, factores, umbrales)
vive aquí para que tocar la ciencia no implique tocar la lógica.
"""

from __future__ import annotations

from typing import Final

# --- Mifflin-St Jeor (TMB) ---
TMB_PESO: Final = 10.0
TMB_ALTURA: Final = 6.25
TMB_EDAD: Final = 5.0
TMB_OFFSET_HOMBRE: Final = 5.0
TMB_OFFSET_MUJER: Final = -161.0

# --- Estimación de % grasa (Deurenberg) ---
GRASA_IMC_COEF: Final = 1.20
GRASA_EDAD_COEF: Final = 0.23
GRASA_SEXO_COEF: Final = 10.8
GRASA_CONST: Final = 5.4
# Edad efectiva máxima para evitar que el término de edad crezca sin límite
GRASA_AJUSTE_EDAD_LIMITE: Final = 36
GRASA_AJUSTE_HOMBRE: Final = 13.0
GRASA_AJUSTE_MUJER: Final = 2.0
# Ajuste negativo (pp) según nivel de entrenamiento (personas entrenadas tienen menos grasa real)
GRASA_AJUSTE_NIVEL: Final = {
    "principiante": 0.0,
    "intermedio": 2.5,
    "avanzado": 5.0,
}

# --- Niveles de actividad (Mifflin-St Jeor) ---
ACTIVIDAD_MIFFLIN: Final = {
    "sedentario": 1.2,
    "ligera": 1.375,
    "moderada": 1.55,
    "alta": 1.725,
    "muy_alta": 1.9,
}

# --- Niveles de actividad (Eric Helms, avanzados) ---
ACTIVIDAD_HELMS: Final = {
    "sedentario": 1.35,
    "ligera": 1.55,
    "activa": 1.75,
    "muy_activa": 1.95,
}

# --- Agresividad (id -> valor numérico) ---
AGRESIVIDAD: Final = {
    "muy_baja": 0.25,
    "baja": 0.4,
    "moderada": 0.6,
    "alta": 0.8,
    "muy_alta": 1.0,
}

# --- Déficit / superávit calórico por nivel (min, max) ---
DEFICIT_POR_NIVEL: Final = {
    "principiante": (250, 500),
    "intermedio": (200, 400),
    "avanzado": (100, 300),
}

SUPERAVIT_POR_NIVEL: Final = {
    "principiante": (250, 500),
    "intermedio": (200, 400),
    "avanzado": (100, 300),
}

# Curva de agresividad usada en volumen para el superávit
AJUSTE_AGRESIVIDAD_VOL_SUPERAVIT: Final = {
    0.25: 0.0,
    0.4: 0.25,
    0.6: 0.5,
    0.8: 0.75,
    1.0: 1.0,
}

# --- Macronutrientes (g/kg) ---
# Evidencia reciente (Helms, Aragon, Phillips, Refalo 2024):
# - Hombre: ~1.6 g/kg de proteína cubre la respuesta máxima de síntesis.
# - Mujer en déficit o recomposición: ligeramente más proteína (~1.8 g/kg)
#   ayuda a preservar masa magra, sobre todo en fase lútea.
# - Mujer: mínimo de ~1.0 g/kg de grasa para no comprometer eje hormonal.
PROTEINA_DEF_G_KG: Final = {"M": 1.6, "F": 1.8}
PROTEINA_VOL_G_KG: Final = {"M": 1.6, "F": 1.6}
PROTEINA_MANT_G_KG: Final = {"M": 1.6, "F": 1.8}

GRASA_DEF_G_KG: Final = {"M": 0.8, "F": 1.0}
GRASA_VOL_G_KG: Final = {"M": 1.0, "F": 1.0}
GRASA_MANT_G_KG: Final = {"M": 0.9, "F": 1.0}

# --- Recomposición corporal (objetivo "mant") ---
# Sesgo calórico por enfoque elegido por el usuario.
RECOMP_AJUSTE_PCT_GET: Final = {
    "musculo": 0.05,  # superávit suave
    "equilibrio": 0.0,  # balance neutro
    "grasa": -0.10,  # déficit suave
}
RECOMP_AJUSTE_MAX_KCAL: Final = 250
# Factor de realismo aplicado a la grasa potencialmente perdida.
RECOMP_PERDIDA_GRASA_FACTOR: Final = {
    "musculo": 0.15,
    "equilibrio": 0.35,
    "grasa": 0.55,
}
# Multiplicador sobre la ganancia muscular semanal según enfoque.
RECOMP_GANANCIA_FACTOR: Final = {
    "musculo": 1.2,
    "equilibrio": 1.0,
    "grasa": 0.75,
}
# Duración mínima recomendada para una recomposición.
DURACION_MIN_RECOMP: Final = 12

KCAL_POR_G_PROTEINA: Final = 4
KCAL_POR_G_GRASA: Final = 9
KCAL_POR_G_CARBO: Final = 4

# --- Cálculo de semanas ---
KCAL_POR_KG_GRASA: Final = 7700
SUAVIZADO_SEMANAS: Final = 1.5

AJUSTE_DURACION_DEF: Final = {
    0.25: 1.00,
    0.4: 0.97,
    0.6: 0.95,
    0.8: 0.93,
    1.0: 0.90,
}

AJUSTE_DURACION_VOL: Final = {
    0.25: 1.08,
    0.4: 1.05,
    0.6: 1.02,
    0.8: 0.98,
    1.0: 0.95,
}

# --- Ganancia muscular base (kg/mes) ---
GANANCIA_MENSUAL_VOL: Final = {
    "principiante": 1.0,
    "intermedio": 0.6,
    "avanzado": 0.3,
}

GANANCIA_MENSUAL_DEF: Final = {
    "principiante": 0.35,
    "intermedio": 0.15,
    "avanzado": 0.05,
}

# Ajuste por agresividad sobre la ganancia muscular
AJUSTE_GANANCIA_VOL_AGRESIVIDAD: Final = {
    0.25: 0.8,
    0.4: 0.9,
    0.6: 1.0,
    0.8: 1.1,
    1.0: 1.2,
}

AJUSTE_GANANCIA_DEF_AGRESIVIDAD: Final = {
    0.25: 1.0,
    0.4: 0.9,
    0.6: 0.8,
    0.8: 0.6,
    1.0: 0.4,
}


# Ajuste por nivel de actividad (umbrales sobre el multiplicador de actividad)
def ajuste_actividad_vol(actividad: float) -> float:
    if actividad <= 1.35:
        return 0.85
    if actividad <= 1.55:
        return 1.0
    if actividad <= 1.75:
        return 1.1
    return 1.15


def ajuste_actividad_def(actividad: float) -> float:
    if actividad <= 1.35:
        return 0.9
    if actividad <= 1.55:
        return 1.0
    if actividad <= 1.75:
        return 1.05
    return 1.1


# Factor sexo sobre la ganancia muscular (mujer ~80% del hombre)
FACTOR_GANANCIA_SEXO_HOMBRE: Final = 1.0
FACTOR_GANANCIA_SEXO_MUJER: Final = 0.8

# --- Duración mínima recomendada (semanas) ---
DURACION_MIN_DEF: Final = {"principiante": 10, "intermedio": 9, "avanzado": 8}
DURACION_MIN_VOL: Final = {"principiante": 16, "intermedio": 14, "avanzado": 12}

# Extensión adicional de volumen si grasa es baja
EXTENSION_VOL_GRASA_BAJA_SEMANAS: Final = 2

# --- Umbrales de % grasa por objetivo y sexo ---
GRASA_OBJ_DEF: Final = {"M": 10, "F": 16}
GRASA_OBJ_MANT: Final = {"M": 14, "F": 22}
GRASA_OBJ_VOL: Final = {"M": 19, "F": 25}  # 19 es el por_grasa_vol original

# Acceso unificado: GRASA_OBJ_POR_OBJETIVO[objetivo][sexo]
GRASA_OBJ_POR_OBJETIVO: Final = {
    "def": GRASA_OBJ_DEF,
    "vol": GRASA_OBJ_VOL,
    "mant": GRASA_OBJ_MANT,
}

# Umbrales para advertencias
GRASA_VOL_LIMITE_ADVERTENCIA: Final = {"M": 16, "F": 25}
GRASA_DEF_LIMITE_ADVERTENCIA: Final = {"M": 10, "F": 16}
GRASA_BAJA_VOL: Final = {"M": 12, "F": 20}
GRASA_RECOMENDAR_DEF: Final = {"M": 16.5, "F": 23.5}
GRASA_RECOMENDAR_VOL: Final = {"M": 12, "F": 20}

# --- Categorías de IMC ---
IMC_CATEGORIAS: Final = [
    (18.5, "Bajo peso"),
    (25.0, "Peso normal"),
    (30.0, "Sobrepeso"),
    (35.0, "Obesidad I"),
    (40.0, "Obesidad II"),
    (float("inf"), "Obesidad III"),
]

# --- Validaciones de entrada ---
LIMITES = {
    "peso": (30, 200),
    "altura": (100, 250),
    "edad": (14, 100),
    "grasa": (5, 50),
}

# --- Ajuste calórico para alargar etapas demasiado cortas ---
AJUSTE_KCAL_BASE_DIA: Final = 50
AJUSTE_KCAL_POR_SEMANA_EXTRA: Final = 30
AJUSTE_KCAL_MAX_SEMANAS_EXTRA: Final = 5

AJUSTE_KCAL_POR_AGRESIVIDAD: Final = {
    0.25: 30,
    0.4: 15,
    0.6: 0,
    0.8: -15,
    1.0: -30,
}


# Factor edad sobre ganancia muscular (función a trozos)
def multiplicador_ganancia_edad(edad: int) -> float:
    if edad < 18:
        return 0.8
    if edad <= 30:
        return 1.0
    if edad <= 40:
        return 1.0 - (edad - 30) * 0.02
    if edad <= 50:
        return 0.9 - (edad - 40) * 0.02
    if edad <= 60:
        return 0.7 - (edad - 50) * 0.02
    return max(0.5 - (edad - 60) * 0.01, 0.3)
