"""Modelos Pydantic para la API de dietas."""

from __future__ import annotations

from typing import Literal

from pydantic import BaseModel, Field, field_validator

from . import constants as C

Supermercado = Literal["mercadona", "carrefour", "lidl", "dia"]
# Mismos ids que backend-kcal: mantenimiento/recomposición, definición, volumen.
Objetivo = Literal["mant", "def", "vol"]
Intolerancia = Literal[
    "gluten", "lactosa", "frutos_secos", "huevo", "pescado", "marisco", "soja", "sesamo"
]
Variedad = Literal["sin_repetir", "repetir_ok"]


class Macros(BaseModel):
    """Gramos de cada macronutriente. Mismos nombres que backend-kcal."""

    proteinas: float = Field(..., ge=0)
    grasas: float = Field(..., ge=0)
    carbohidratos: float = Field(..., ge=0)


class DietInput(BaseModel):
    """Entrada de POST /diet/generate.

    kcal/macros/objetivo vienen ya calculados por backend-kcal (/plan);
    este servicio no recalcula nada.
    """

    kcal_objetivo: float = Field(..., gt=0, description="kcal diarias objetivo")
    macros_objetivo: Macros
    objetivo: Objetivo
    supermercado: Supermercado
    num_comidas: int = Field(
        4, ge=C.NUM_COMIDAS_MIN, le=C.NUM_COMIDAS_MAX, description="Comidas al día"
    )
    intolerancias: list[Intolerancia] = []
    evitar: list[str] = Field(default=[], description="Alimentos que no deben aparecer")
    favoritos: list[str] = Field(default=[], description="Alimentos a priorizar")
    variedad: Variedad = "sin_repetir"

    @field_validator("evitar", "favoritos")
    @classmethod
    def _limpiar_listas(cls, v: list[str]) -> list[str]:
        return [s.strip().lower() for s in v if s.strip()]


# ---- Dataset / RAG ----


class Producto(BaseModel):
    """Un producto del dataset (macros por 100 g, datos reales de OFF)."""

    code: str
    nombre: str
    marca: str = ""
    supermercado: Supermercado
    categoria: str
    kcal_100g: float
    proteinas_100g: float
    grasas_100g: float
    carbohidratos_100g: float
    alergenos: list[str] = []
    # popularity_key de OFF: cuanto más alto, más escaneado/conocido.
    popularidad: int = 0


# ---- Salidas estructuradas del LLM (grafo) ----


class RepartoComida(BaseModel):
    """Objetivo de kcal/macros para una comida concreta (salida del Planner)."""

    nombre: str
    kcal: float = Field(..., ge=0)
    proteinas: float = Field(..., ge=0)
    grasas: float = Field(..., ge=0)
    carbohidratos: float = Field(..., ge=0)


class RepartoLLM(BaseModel):
    """Salida estructurada del Planner: reparto del día en comidas."""

    comidas: list[RepartoComida]


class ItemComposer(BaseModel):
    """Un producto elegido por el Composer: solo código y gramos.

    Las kcal/macros NUNCA las escribe el LLM: se calculan en Python a
    partir del dataset."""

    code: str
    cantidad_g: float


class ComidaComposer(BaseModel):
    nombre: str
    items: list[ItemComposer]


class PlanComposerLLM(BaseModel):
    """Salida estructurada del Composer."""

    comidas: list[ComidaComposer]


# ---- Respuesta ----


class ProductoPlan(BaseModel):
    """Un producto concreto dentro de una comida, con su cantidad."""

    nombre: str
    marca: str = ""
    cantidad_g: float
    kcal: float
    proteinas: float
    grasas: float
    carbohidratos: float


class Comida(BaseModel):
    nombre: str
    productos: list[ProductoPlan]
    kcal: float
    macros: Macros


class Desviacion(BaseModel):
    """Desviación relativa del plan frente al objetivo (positiva = exceso)."""

    kcal_pct: float
    proteinas_pct: float
    grasas_pct: float
    carbohidratos_pct: float
    dentro_tolerancia: bool


class MejorPlan(BaseModel):
    """Mejor intento visto hasta ahora en el bucle de validación."""

    comidas: list[Comida]
    desviacion: Desviacion
    puntuacion: float


class DietOutput(BaseModel):
    comidas: list[Comida]
    kcal_total: float
    macros_totales: Macros
    desviacion: Desviacion
    intentos_usados: int
    # True si se agotaron los reintentos y el plan queda fuera de tolerancia.
    aproximado: bool
    pdf_base64: str | None = None
