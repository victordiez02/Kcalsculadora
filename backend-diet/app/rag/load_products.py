"""Carga del dataset estático de productos (CSVs generados por
scripts/build_dataset.py) como modelos Pydantic."""

from __future__ import annotations

import csv
from pathlib import Path

from .. import constants as C
from ..schemas import Producto

DATA_DIR = Path(__file__).parent / "data"


def cargar_productos(supermercado: str | None = None) -> list[Producto]:
    """Carga los productos de un supermercado (o de todos si es None)."""
    ids = [supermercado] if supermercado else list(C.SUPERMERCADOS)
    productos: list[Producto] = []
    for sid in ids:
        ruta = DATA_DIR / f"{sid}.csv"
        if not ruta.exists():
            raise FileNotFoundError(
                f"No existe el dataset {ruta}. Ejecuta scripts/build_dataset.py primero."
            )
        with ruta.open(encoding="utf-8", newline="") as f:
            for fila in csv.DictReader(f):
                productos.append(
                    Producto(
                        code=fila["code"],
                        nombre=fila["nombre"],
                        marca=fila["marca"],
                        supermercado=sid,  # type: ignore[arg-type]
                        categoria=fila["categoria"],
                        kcal_100g=float(fila["kcal_100g"]),
                        proteinas_100g=float(fila["proteinas_100g"]),
                        grasas_100g=float(fila["grasas_100g"]),
                        carbohidratos_100g=float(fila["carbohidratos_100g"]),
                        alergenos=[a for a in fila["alergenos"].split("|") if a],
                        popularidad=int(fila.get("popularidad", 0) or 0),
                    )
                )
    return productos
