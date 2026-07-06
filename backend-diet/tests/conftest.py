"""Fixtures compartidas: estados y productos fabricados, sin LLM ni Chroma."""

from __future__ import annotations

import pytest

from app.schemas import Comida, DietInput, Macros, Producto, ProductoPlan


@pytest.fixture
def entrada() -> DietInput:
    return DietInput(
        kcal_objetivo=2000,
        macros_objetivo=Macros(proteinas=150, grasas=60, carbohidratos=215),
        objetivo="def",
        supermercado="mercadona",
        num_comidas=3,
    )


def hacer_producto(**kwargs) -> Producto:
    base = {
        "code": "1",
        "nombre": "Producto test",
        "marca": "Marca",
        "supermercado": "mercadona",
        "categoria": "yogures",
        "kcal_100g": 100.0,
        "proteinas_100g": 10.0,
        "grasas_100g": 2.0,
        "carbohidratos_100g": 10.0,
        "alergenos": [],
    }
    base.update(kwargs)
    return Producto(**base)


def hacer_comida(nombre: str, kcal: float, prot: float, grasa: float, carbo: float) -> Comida:
    return Comida(
        nombre=nombre,
        productos=[
            ProductoPlan(
                nombre="X",
                cantidad_g=100,
                kcal=kcal,
                proteinas=prot,
                grasas=grasa,
                carbohidratos=carbo,
            )
        ],
        kcal=kcal,
        macros=Macros(proteinas=prot, grasas=grasa, carbohidratos=carbo),
    )
