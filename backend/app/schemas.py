"""Modelos Pydantic para la API."""

from __future__ import annotations

from typing import Literal

from pydantic import BaseModel, Field, field_validator

from . import constants as C

Sexo = Literal["M", "F"]
Nivel = Literal["principiante", "intermedio", "avanzado"]
Objetivo = Literal["mant", "def", "vol"]
Agresividad = Literal["muy_baja", "baja", "moderada", "alta", "muy_alta"]
EnfoqueRecomp = Literal["musculo", "equilibrio", "grasa"]


def _check_rango(campo: str, valor: float, unidad: str) -> float:
    lo, hi = C.LIMITES[campo]
    if not lo <= valor <= hi:
        raise ValueError(f"El {campo} debe estar entre {lo} y {hi} {unidad}.")
    return valor


class CalculoInput(BaseModel):
    peso: float = Field(..., description="kg")
    altura: float = Field(..., description="cm")
    edad: int
    sexo: Sexo
    grasa: float | None = Field(
        None, description="% de grasa corporal. Si es None se estima."
    )
    nivel: Nivel = "principiante"
    actividad: float = Field(..., description="Factor de actividad (1.2-1.95)")
    objetivo: Objetivo = "mant"
    agresividad: Agresividad = "moderada"
    enfoque_recomp: EnfoqueRecomp = "equilibrio"

    @field_validator("peso")
    @classmethod
    def _v_peso(cls, v: float) -> float:
        return _check_rango("peso", v, "kg")

    @field_validator("altura")
    @classmethod
    def _v_altura(cls, v: float) -> float:
        return _check_rango("altura", v, "cm")

    @field_validator("edad")
    @classmethod
    def _v_edad(cls, v: int) -> int:
        return int(_check_rango("edad", v, "años"))

    @field_validator("grasa")
    @classmethod
    def _v_grasa(cls, v: float | None) -> float | None:
        return v if v is None else _check_rango("grasa", v, "%")


class Macros(BaseModel):
    proteinas: float
    grasas: float
    carbohidratos: float


class Aviso(BaseModel):
    nivel: Literal["info", "warning", "danger"]
    mensaje: str


class AjusteRecomendado(BaseModel):
    """Se devuelve cuando las semanas calculadas son menores al mínimo
    recomendado y conviene suavizar el plan."""

    calorias: float
    macros: Macros
    semanas_minimas: int
    delta_kcal_dia: float
    gramos_por_semana: float
    motivo: str


class CalculoOutput(BaseModel):
    imc: float
    imc_categoria: str
    tmb: float
    get: float
    masa_magra: float
    grasa_estimada: float
    grasa_es_estimada: bool
    peso_objetivo: float
    calorias_recomendadas: float
    macros: Macros
    semanas_necesarias: float | None
    semanas_minimas: int
    ajuste_calorico: float
    tipo_ajuste: Literal["reducir", "aumentar", "mantener"]
    ganancia_muscular_kg: float
    perdida_grasa_kg: float
    grasa_objetivo: float
    avisos: list[Aviso]
    ajuste_recomendado: AjusteRecomendado | None = None
    # Datos crudos útiles para el frontend
    factor_actividad: float
    detalle: Detalle


class Detalle(BaseModel):
    """Valores intermedios usados en cada fórmula. Permite al frontend
    mostrar el cálculo paso a paso con números explícitos."""

    # TMB (Mifflin-St Jeor)
    tmb_coef_peso: float
    tmb_coef_altura: float
    tmb_coef_edad: float
    tmb_offset: float
    # Grasa (Deurenberg)
    grasa_coef_imc: float
    grasa_coef_edad: float
    grasa_coef_sexo: float
    grasa_const: float
    grasa_sexo_val: int
    grasa_edad_efectiva: int
    grasa_ajuste_nivel: float
    # Macros
    proteina_g_kg: float
    grasa_g_kg: float
    kcal_por_g_proteina: int
    kcal_por_g_grasa: int
    kcal_por_g_carbo: int
    # Calorías (def / vol)
    agresividad_valor: float | None = None
    deficit_rango: list[float] | None = None
    superavit_rango: list[float] | None = None
    superavit_factor: float | None = None
    # Recomposición
    recomp_pct_get: float | None = None
    recomp_ajuste_max: float | None = None
    recomp_ganancia_factor: float | None = None
    recomp_perdida_grasa_factor: float | None = None
    # Ganancia muscular semanal
    ganancia_base_mensual: float
    ganancia_aj_agresividad: float
    ganancia_aj_actividad: float
    ganancia_factor_sexo: float
    ganancia_factor_edad: float
    ganancia_semanal_kg: float
    # Semanas necesarias
    kcal_por_kg_grasa: int
    suavizado_semanas: float
    ajuste_duracion: float | None = None


CalculoOutput.model_rebuild()


# ---- Schemas de los endpoints atómicos ----


class _MedidasBase(BaseModel):
    peso: float = Field(..., gt=0)
    altura: float = Field(..., gt=0, description="cm")


class GrasaInput(_MedidasBase):
    edad: int = Field(..., ge=1)
    sexo: Sexo
    nivel: Nivel = "principiante"


class GrasaOutput(BaseModel):
    grasa_estimada: float
    masa_magra: float


class TmbInput(_MedidasBase):
    edad: int = Field(..., ge=1)
    sexo: Sexo


class TmbOutput(BaseModel):
    tmb: float


class GetInput(BaseModel):
    tmb: float = Field(..., gt=0)
    actividad: float = Field(..., gt=0)


class GetOutput(BaseModel):
    get: float


class ImcInput(_MedidasBase):
    pass


class ImcOutput(BaseModel):
    imc: float
    categoria: str


class CaloriasInput(BaseModel):
    objetivo: Objetivo
    nivel: Nivel
    agresividad: Agresividad = "moderada"
    get: float = Field(..., gt=0)
    enfoque_recomp: EnfoqueRecomp | None = None


class CaloriasOutput(BaseModel):
    calorias_recomendadas: float
    ajuste_calorico: float
    tipo_ajuste: Literal["reducir", "aumentar", "mantener"]


class MacrosInput(BaseModel):
    peso: float = Field(..., gt=0)
    peso_objetivo: float = Field(..., gt=0)
    calorias: float = Field(..., gt=0)
    objetivo: Objetivo
    sexo: Sexo


class PesoObjetivoInput(BaseModel):
    peso: float
    masa_magra: float
    incremento_masa_magra: float = 0
    objetivo: Objetivo
    sexo: Sexo
    grasa: float


class PesoObjetivoOutput(BaseModel):
    peso_objetivo: float
    grasa_objetivo: float


class SemanasInput(BaseModel):
    objetivo: Objetivo
    agresividad: Agresividad = "moderada"
    peso: float
    peso_objetivo: float
    calorias_recomendadas: float
    get: float


class SemanasOutput(BaseModel):
    semanas_necesarias: float | None
