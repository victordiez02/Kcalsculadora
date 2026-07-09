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
# Cuántos menús diarios distintos tendrá la semana (ver C.VARIEDADES).
Variedad = Literal["baja", "media", "alta"]
# Categorías internas de alimento. Debe coincidir con C.CATEGORIAS (hay un
# test que lo garantiza); se duplica como Literal para que el JSON schema del
# LLM lleve el enum y el Chef no pueda inventar categorías.
CategoriaAlimento = Literal[
    "cereales_desayuno",
    "yogures",
    "quesos",
    "lacteos",
    "huevos",
    "carne",
    "embutidos",
    "pescado_marisco",
    "legumbres",
    "pasta_arroz",
    "pan",
    "fruta",
    "verdura",
    "frutos_secos_semillas",
    "aceites_salsas",
    "platos_preparados",
    "snacks_dulces",
    "bebidas",
]


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
    variedad: Variedad = "media"

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


class IngredienteChef(BaseModel):
    """Un ingrediente genérico de un plato (salida del Chef).

    La cantidad es solo orientativa: define las proporciones del plato y las
    cotas del ajuste; los gramos finales los calcula el ajustador en Python,
    nunca el LLM."""

    nombre: str = Field(..., description="Alimento genérico sin marca, ej. 'pechuga de pollo'")
    categoria: CategoriaAlimento
    cantidad_g: float = Field(..., gt=0, description="Cantidad orientativa en gramos")


class PlatoChef(BaseModel):
    """Un plato con nombre para una comida del día (salida del Chef)."""

    comida: str
    nombre: str = Field(..., description="Nombre apetecible del plato")
    ingredientes: list[IngredienteChef]


class MenuChefLLM(BaseModel):
    """Salida estructurada del Chef: un plato por comida."""

    platos: list[PlatoChef]


# ---- Resultado del Retriever: ingredientes mapeados a productos reales ----


class IngredienteResuelto(BaseModel):
    """Un ingrediente del Chef ya mapeado a un producto real del súper."""

    ingrediente: IngredienteChef
    producto: Producto


class PlatoResuelto(BaseModel):
    comida: str
    nombre: str
    ingredientes: list[IngredienteResuelto]


# ---- Respuesta ----


class ProductoPlan(BaseModel):
    """Un producto concreto dentro de una comida, con su cantidad."""

    # Ingrediente genérico del plato al que corresponde ("arroz", "salmón").
    ingrediente: str = ""
    nombre: str
    marca: str = ""
    cantidad_g: float
    kcal: float
    proteinas: float
    grasas: float
    carbohidratos: float


class Comida(BaseModel):
    nombre: str
    # Nombre del plato ("Salmón al horno con arroz y brócoli").
    plato: str = ""
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


# ---- Plan semanal ----


class MenuDia(BaseModel):
    """Un menú diario concreto y los días de la semana que lo usan."""

    dias: list[str]  # p. ej. ["Lunes", "Viernes"]
    comidas: list[Comida]
    kcal: float
    macros: Macros
    desviacion: Desviacion
    # True si este menú quedó fuera de tolerancia tras agotar los intentos.
    aproximado: bool


class Semana(BaseModel):
    """Plan semanal: menús distintos y su reparto en los 7 días.

    `menus[0]` es siempre el del lunes."""

    menus: list[MenuDia]
    kcal_diarias: float  # media real por día de la semana
    macros_diarios: Macros
    intentos_usados: int
    aproximado: bool  # algún menú quedó fuera de tolerancia


class DietOutput(BaseModel):
    """Respuesta de /diet/generate. El plan completo va solo en el PDF; la
    app muestra el resumen diario y el lunes como ejemplo."""

    kcal_diarias: float
    macros_diarios: Macros
    menu_ejemplo: MenuDia  # el del lunes
    menus_distintos: int
    intentos_usados: int
    aproximado: bool
    pdf_base64: str | None = None
