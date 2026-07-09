"""Tests de los filtros deterministas del Retriever (sin vector store)."""

from __future__ import annotations

from app.graph.nodes import (
    _elegir_producto,
    _filtrar_evitar,
    _pasa_salvaguarda_intolerancia,
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


def test_elegir_producto_toma_el_mas_popular_que_pasa_filtros(entrada):
    normal = hacer_producto(code="n", nombre="Yogur natural", popularidad=100)
    popular = hacer_producto(code="p", nombre="Yogur griego", popularidad=9999)
    seleccion = _elegir_producto([normal, popular], entrada)
    assert seleccion is not None and seleccion.code == "p"


def test_elegir_producto_respeta_evitar(entrada):
    entrada = entrada.model_copy(update={"evitar": ["queso"]})
    queso = hacer_producto(code="q", nombre="Queso curado", popularidad=9999)
    yogur = hacer_producto(code="y", nombre="Yogur natural", popularidad=1)
    seleccion = _elegir_producto([queso, yogur], entrada)
    assert seleccion is not None and seleccion.code == "y"
    assert _elegir_producto([queso], entrada) is None
