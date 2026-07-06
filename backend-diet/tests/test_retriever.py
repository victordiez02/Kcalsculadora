"""Tests de los filtros deterministas del Retriever (sin vector store)."""

from __future__ import annotations

from app import constants as C
from app.graph.nodes import (
    _filtrar_evitar,
    _pasa_salvaguarda_intolerancia,
    _seleccionar_candidatos,
)

from .conftest import hacer_producto


def test_filtrar_evitar_ignora_tildes_y_mayusculas():
    atun = hacer_producto(code="1", nombre="Atún claro en aceite")
    yogur = hacer_producto(code="2", nombre="Yogur natural")
    assert _filtrar_evitar([atun, yogur], ["atun"]) == [yogur]


def test_salvaguarda_lactosa_bloquea_lacteo_sin_alergenos_declarados():
    yogur_sospechoso = hacer_producto(nombre="Yogur natural", categoria="yogures", alergenos=[])
    assert not _pasa_salvaguarda_intolerancia(yogur_sospechoso, ["lactosa"])


def test_salvaguarda_lactosa_acepta_marcador_explicito():
    sin_lactosa = hacer_producto(nombre="Yogur natural sin lactosa", categoria="yogures")
    vegetal = hacer_producto(nombre="Bebida de avena", categoria="lacteos")
    assert _pasa_salvaguarda_intolerancia(sin_lactosa, ["lactosa"])
    assert _pasa_salvaguarda_intolerancia(vegetal, ["lactosa"])


def test_salvaguarda_no_aplica_fuera_de_categorias_de_riesgo():
    fruta = hacer_producto(nombre="Manzana", categoria="fruta", alergenos=[])
    assert _pasa_salvaguarda_intolerancia(fruta, ["lactosa", "gluten"])


def test_salvaguarda_confia_en_alergenos_declarados():
    # Declara alérgenos (sin en:milk): el filtro del vector store ya hizo su parte.
    yogur_soja = hacer_producto(
        nombre="Yogur de soja", categoria="yogures", alergenos=["en:soybeans"]
    )
    assert _pasa_salvaguarda_intolerancia(yogur_soja, ["lactosa"])


def test_seleccionar_candidatos_prioriza_favoritos_y_popularidad(entrada):
    normal = hacer_producto(code="n", nombre="Queso fresco", popularidad=100)
    popular = hacer_producto(code="p", nombre="Yogur griego", popularidad=9999)
    favorito = hacer_producto(code="f", nombre="Skyr", popularidad=1)
    seleccion = _seleccionar_candidatos([normal, popular], entrada, [favorito])
    assert [p.code for p in seleccion] == ["f", "p", "n"]


def test_seleccionar_candidatos_respeta_evitar_y_tope(entrada):
    entrada = entrada.model_copy(update={"evitar": ["queso"]})
    productos = [hacer_producto(code=str(i), nombre=f"Yogur {i}") for i in range(50)]
    productos.append(hacer_producto(code="q", nombre="Queso curado"))
    seleccion = _seleccionar_candidatos(productos, entrada, [])
    assert len(seleccion) == C.N_CANDIDATOS_POR_COMIDA
    assert all("queso" not in p.nombre.lower() for p in seleccion)
