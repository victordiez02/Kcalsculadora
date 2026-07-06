"""Construye el dataset estático de productos por supermercado.

Uso (offline, no forma parte del servicio):

    python scripts/build_dataset.py [--parquet RUTA_LOCAL]

Lee el dump Parquet de Open Food Facts (por defecto el publicado en
Hugging Face; con --parquet se usa una copia local descargada, mucho más
rápido si se itera), filtra productos vendidos en España en los
supermercados soportados, limpia filas con macros incompletos o
incoherentes y escribe un CSV por supermercado en app/rag/data/.
"""

from __future__ import annotations

import argparse
import csv
import sys
from collections import Counter
from pathlib import Path

import duckdb

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from app import constants as C

DATA_DIR = Path(__file__).resolve().parents[1] / "app" / "rag" / "data"
DEFAULT_PARQUET = "hf://datasets/openfoodfacts/product-database/food.parquet"

SQL = """
SELECT
  code,
  product_name,
  brands,
  categories_tags,
  allergens_tags,
  stores_tags,
  coalesce(popularity_key, 0) AS popularidad,
  list_filter(nutriments, x -> x.name = 'energy-kcal')[1]."100g" AS kcal_100g,
  list_filter(nutriments, x -> x.name = 'proteins')[1]."100g"    AS proteinas_100g,
  list_filter(nutriments, x -> x.name = 'fat')[1]."100g"         AS grasas_100g,
  list_filter(nutriments, x -> x.name = 'carbohydrates')[1]."100g" AS carbohidratos_100g
FROM read_parquet($parquet)
WHERE list_contains(countries_tags, 'en:spain')
  AND (obsolete IS NULL OR NOT obsolete)
  AND len(list_intersect(stores_tags, $store_tags)) > 0
  AND nutriments IS NOT NULL
"""


def _nombre(product_name: list[dict] | None) -> str:
    """Prefiere el nombre en español; si no, el principal; si no, el primero."""
    if not product_name:
        return ""
    por_lang = {e["lang"]: (e["text"] or "").strip() for e in product_name}
    return por_lang.get("es") or por_lang.get("main") or next(iter(por_lang.values()), "")


def _categoria(tags: list[str] | None) -> str | None:
    for interna, off_tags in C.CATEGORIA_OFF_MAP:
        if any(t in (tags or []) for t in off_tags):
            return interna
    return None


def _sanity_ok(kcal: float, prot: float, grasa: float, carbo: float) -> bool:
    if not (C.KCAL_100G_MIN <= kcal <= C.KCAL_100G_MAX):
        return False
    if any(not (0 <= m <= C.MACROS_100G_MAX) for m in (prot, grasa, carbo)):
        return False
    if prot + grasa + carbo > C.SUMA_MACROS_100G_MAX:
        return False
    kcal_estimada = (
        prot * C.KCAL_POR_G["proteinas"]
        + grasa * C.KCAL_POR_G["grasas"]
        + carbo * C.KCAL_POR_G["carbohidratos"]
    )
    margen = max(C.SANITY_KCAL_TOL_ABS, kcal_estimada * C.SANITY_KCAL_TOL_PCT)
    return abs(kcal - kcal_estimada) <= margen


def construir(parquet: str) -> None:
    todos_los_tags = [t for s in C.SUPERMERCADOS.values() for t in s["store_tags"]]
    con = duckdb.connect()
    filas = con.execute(SQL, {"parquet": parquet, "store_tags": todos_los_tags}).fetchall()
    print(f"Filas crudas (España + supermercados soportados): {len(filas)}")

    por_super: dict[str, dict[str, dict]] = {sid: {} for sid in C.SUPERMERCADOS}
    descartes: Counter = Counter()

    for (
        code,
        product_name,
        brands,
        cat_tags,
        alerg_tags,
        store_tags,
        popularidad,
        kcal,
        prot,
        grasa,
        carbo,
    ) in filas:
        if kcal is None or prot is None or grasa is None or carbo is None:
            descartes["macros_incompletos"] += 1
            continue
        nombre = _nombre(product_name)
        if not nombre:
            descartes["sin_nombre"] += 1
            continue
        categoria = _categoria(cat_tags)
        if categoria is None:
            descartes["sin_categoria"] += 1
            continue
        if not _sanity_ok(kcal, prot, grasa, carbo):
            descartes["macros_incoherentes"] += 1
            continue

        fila = {
            "code": str(code),
            "nombre": nombre,
            "marca": (brands or "").strip(),
            "categoria": categoria,
            "kcal_100g": round(kcal, 1),
            "proteinas_100g": round(prot, 1),
            "grasas_100g": round(grasa, 1),
            "carbohidratos_100g": round(carbo, 1),
            "alergenos": "|".join(alerg_tags or []),
            "popularidad": int(popularidad),
        }
        tags = set(store_tags or [])
        for sid, s in C.SUPERMERCADOS.items():
            if tags & set(s["store_tags"]):
                por_super[sid][str(code)] = fila  # dedupe por código de barras

    DATA_DIR.mkdir(parents=True, exist_ok=True)
    columnas = [
        "code",
        "nombre",
        "marca",
        "categoria",
        "kcal_100g",
        "proteinas_100g",
        "grasas_100g",
        "carbohidratos_100g",
        "alergenos",
        "popularidad",
    ]
    for sid, productos in por_super.items():
        ruta = DATA_DIR / f"{sid}.csv"
        with ruta.open("w", encoding="utf-8", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=columnas)
            writer.writeheader()
            for fila in sorted(productos.values(), key=lambda x: x["nombre"]):
                writer.writerow(fila)
        cats = Counter(p["categoria"] for p in productos.values())
        print(f"{sid}: {len(productos)} productos -> {ruta.name}")
        print(f"  top categorías: {cats.most_common(6)}")
    print(f"Descartes: {dict(descartes)}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--parquet", default=DEFAULT_PARQUET)
    construir(parser.parse_args().parquet)
