"""Constantes y parámetros del servicio de generación de dietas.

Toda la configuración (tolerancias, reintentos, catálogos, mapeos del
dataset) vive aquí para que ajustar el comportamiento no implique tocar
la lógica.
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import Final

from dotenv import load_dotenv

# Carga backend-diet/.env antes de leer cualquier variable de entorno.
load_dotenv(Path(__file__).resolve().parents[1] / ".env")

# --- LLM (OpenAI vía LangChain) ---
OPENAI_MODEL: Final = os.environ.get("OPENAI_MODEL") or "gpt-4o-mini"

# --- Supermercados soportados (id -> etiqueta y tags de `stores` en OFF) ---
SUPERMERCADOS: Final = {
    "mercadona": {"label": "Mercadona", "store_tags": ["mercadona"]},
    "carrefour": {
        "label": "Carrefour",
        "store_tags": ["carrefour", "carrefour-market", "carrefour-express", "carrefour-es"],
    },
    "lidl": {"label": "Lidl", "store_tags": ["lidl"]},
    "dia": {"label": "Dia", "store_tags": ["dia", "dia-supermercados", "supermercados-dia"]},
}

# --- Comidas del día ---
NUM_COMIDAS_MIN: Final = 3
NUM_COMIDAS_MAX: Final = 5
# Nombres de las comidas según cuántas haya elegido el usuario.
NOMBRES_COMIDAS: Final = {
    3: ["Desayuno", "Comida", "Cena"],
    4: ["Desayuno", "Comida", "Merienda", "Cena"],
    5: ["Desayuno", "Almuerzo", "Comida", "Merienda", "Cena"],
}

# --- Tolerancias del Validator ---
# Se aplican por menú diario: a las kcal totales Y a cada macro por separado.
TOLERANCIA_KCAL_PCT: Final = 0.02
TOLERANCIA_MACRO_PCT: Final = 0.05
# Nº máximo de intentos de composición antes de devolver el mejor resultado.
MAX_INTENTOS: Final = 3
# Si la desviación de kcal supera este umbral, se vuelve al Planner en vez
# de reintentar solo el Chef.
UMBRAL_REPLANIFICAR_PCT: Final = 0.20

KCAL_POR_G: Final = {"proteinas": 4, "grasas": 9, "carbohidratos": 4}

# --- Intolerancias (id -> etiqueta y tags de `allergens` en OFF) ---
INTOLERANCIAS: Final = {
    "gluten": {"label": "Gluten", "allergen_tags": ["en:gluten"]},
    "lactosa": {"label": "Lácteos", "allergen_tags": ["en:milk"]},
    "frutos_secos": {"label": "Frutos secos", "allergen_tags": ["en:nuts", "en:peanuts"]},
    "huevo": {"label": "Huevo", "allergen_tags": ["en:eggs"]},
    "pescado": {"label": "Pescado", "allergen_tags": ["en:fish"]},
    "marisco": {"label": "Marisco", "allergen_tags": ["en:crustaceans", "en:molluscs"]},
    "soja": {"label": "Soja", "allergen_tags": ["en:soybeans"]},
    "sesamo": {"label": "Sésamo", "allergen_tags": ["en:sesame-seeds"]},
}

# --- Plan semanal ---
DIAS_SEMANA: Final = ["Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"]
# La variedad elegida decide cuántos menús diarios DISTINTOS se generan; los
# 7 días de la semana se cubren repitiéndolos en ciclo (día d -> menú d % n).
VARIEDADES: Final = {
    "baja": {"label": "Poca variedad", "menus": 2},
    "media": {"label": "Variedad media", "menus": 4},
    "alta": {"label": "Máxima variedad", "menus": 7},
}

# --- Categorías internas de alimento ---
# El dataset asigna a cada producto UNA categoría interna a partir de sus
# `categories_tags` de OFF. El orden importa: gana la primera que matchee
# (las más específicas van antes).
CATEGORIA_OFF_MAP: Final = [
    ("cereales_desayuno", ["en:breakfast-cereals", "en:mueslis", "en:oat-flakes", "en:porridge"]),
    ("yogures", ["en:yogurts", "en:fermented-milk-products", "en:skyrs", "en:kefirs"]),
    ("quesos", ["en:cheeses"]),
    (
        "lacteos",
        ["en:dairies", "en:milks", "en:dairy-substitutes", "en:plant-based-milk-alternatives"],
    ),
    ("huevos", ["en:eggs", "en:chicken-eggs"]),
    ("carne", ["en:meats", "en:poultries", "en:meat-preparations", "en:meat-alternatives"]),
    ("embutidos", ["en:cold-cuts", "en:hams", "en:sausages", "en:charcuteries"]),
    ("pescado_marisco", ["en:seafood", "en:fishes", "en:canned-fishes", "en:seafood-preparations"]),
    ("legumbres", ["en:legumes", "en:pulses", "en:canned-legumes", "en:tofu"]),
    ("pasta_arroz", ["en:pastas", "en:rices", "en:cereal-grains", "en:quinoa", "en:couscous"]),
    ("pan", ["en:breads", "en:toasts", "en:rice-cakes", "en:crispbreads"]),
    ("fruta", ["en:fruits", "en:fresh-fruits", "en:canned-fruits", "en:fruit-purees"]),
    (
        "verdura",
        [
            "en:vegetables",
            "en:fresh-vegetables",
            "en:canned-vegetables",
            "en:frozen-vegetables",
            "en:salads",
            "en:mushrooms",
        ],
    ),
    ("frutos_secos_semillas", ["en:nuts", "en:seeds", "en:nut-butters", "en:dried-fruits"]),
    (
        "aceites_salsas",
        ["en:fats", "en:olive-oils", "en:vegetable-oils", "en:sauces", "en:condiments"],
    ),
    ("platos_preparados", ["en:meals", "en:prepared-meats", "en:soups"]),
    (
        "snacks_dulces",
        [
            "en:snacks",
            "en:biscuits",
            "en:chocolates",
            "en:sweets",
            "en:pastries",
            "en:desserts",
            "en:crisps",
        ],
    ),
    ("bebidas", ["en:beverages", "en:plant-based-beverages", "en:juices"]),
]

CATEGORIAS: Final = [c for c, _ in CATEGORIA_OFF_MAP]

# Etiquetas legibles de cada categoría (para embeddings y para el PDF).
CATEGORIA_LABELS: Final = {
    "cereales_desayuno": "Cereales de desayuno",
    "yogures": "Yogures y fermentados",
    "quesos": "Quesos",
    "lacteos": "Lácteos y bebidas vegetales",
    "huevos": "Huevos",
    "carne": "Carne y aves",
    "embutidos": "Embutidos y fiambres",
    "pescado_marisco": "Pescado y marisco",
    "legumbres": "Legumbres y tofu",
    "pasta_arroz": "Pasta, arroz y cereales",
    "pan": "Pan y tostas",
    "fruta": "Fruta",
    "verdura": "Verdura y ensaladas",
    "frutos_secos_semillas": "Frutos secos y semillas",
    "aceites_salsas": "Aceites y salsas",
    "platos_preparados": "Platos preparados",
    "snacks_dulces": "Snacks y dulces",
    "bebidas": "Bebidas",
}

# --- Retrieval ---
# Nº de resultados pedidos al vector store por ingrediente antes de filtrar;
# entre los que sobreviven a los filtros se elige el más popular.
N_RESULTADOS_POR_INGREDIENTE: Final = 8

# Salvaguarda de intolerancias: OFF tiene productos sin alérgenos declarados.
# En estas categorías "de riesgo", si el producto no declara alérgenos solo se
# acepta cuando su nombre contiene un marcador explícito de seguridad.
CATEGORIAS_RIESGO_INTOLERANCIA: Final = {
    "lactosa": ["yogures", "quesos", "lacteos", "cereales_desayuno"],
    "gluten": ["pan", "pasta_arroz", "cereales_desayuno", "snacks_dulces"],
    "huevo": [],
    "frutos_secos": ["frutos_secos_semillas"],
}
MARCADORES_SIN_INTOLERANCIA: Final = {
    "lactosa": ["sin lactosa", "vegetal", "soja", "avena", "almendra", "coco", "arroz"],
    "gluten": ["sin gluten", "gluten free"],
    "huevo": [],
    "frutos_secos": [],
}

# --- Chef ---
# Nº de ingredientes por plato que se le pide al Chef. Un plato que llegue al
# ajustador con menos de INGREDIENTES_POR_PLATO_MIN resueltos dispara un
# reintento del Chef (los ingredientes propuestos no existen en el súper).
INGREDIENTES_POR_PLATO_MIN: Final = 2
INGREDIENTES_POR_PLATO_MAX: Final = 6

# --- Ajustador de cantidades (determinista, sin LLM) ---
# Cantidades permitidas por producto (gramos) y redondeo.
CANTIDAD_MIN_G: Final = 10
CANTIDAD_MAX_G: Final = 500
REDONDEO_CANTIDAD_G: Final = 5
# Cotas del ajuste alrededor de la cantidad orientativa del Chef: los gramos
# finales quedan en [orientativa * MIN, orientativa * MAX] para que el plato
# siga siendo realista (no convertir 10 g de aceite en 200 g).
AJUSTE_FACTOR_MIN: Final = 0.4
AJUSTE_FACTOR_MAX: Final = 2.5
# Suelo del divisor al normalizar el error relativo (evita dividir por ~0
# cuando algún objetivo de macro de una comida es 0).
AJUSTE_DIVISOR_MINIMO: Final = 1.0

# --- Puntuación del mejor intento (Validator) ---
# La desviación de kcal pesa más que la de cada macro individual.
PESO_DESVIACION_KCAL: Final = 2.0

# --- Vector store ---
# Índice Chroma pre-construido y VERSIONADO en el repo (backend-diet/vectorstore/).
# Se regenera con scripts/build_vectorstore.py solo cuando cambian los CSVs;
# el manifiesto guarda los hashes de los CSVs con los que se indexó.
VECTORSTORE_DIR: Final = os.environ.get(
    "VECTORSTORE_DIR",
    os.path.normpath(os.path.join(os.path.dirname(__file__), "..", "vectorstore")),
)
CHROMA_DIR: Final = os.path.join(VECTORSTORE_DIR, "chroma")
VECTORSTORE_MANIFEST: Final = os.path.join(VECTORSTORE_DIR, "manifest.json")
CHROMA_COLLECTION: Final = "productos"

# --- Limpieza del dataset (scripts/build_dataset.py) ---
# Rango físicamente plausible por 100 g.
KCAL_100G_MAX: Final = 950
MACROS_100G_MAX: Final = 100
SUMA_MACROS_100G_MAX: Final = 105
# La kcal declarada debe cuadrar con los macros (4/4/9) dentro de este margen.
SANITY_KCAL_TOL_PCT: Final = 0.30
SANITY_KCAL_TOL_ABS: Final = 30
# Kcal mínima para que un producto sea relevante como alimento.
KCAL_100G_MIN: Final = 5
