"""Indexa el dataset de productos en el vector store Chroma local.

Solo hace falta ejecutarlo cuando cambian los CSVs de datos (tras
regenerarlos con build_dataset.py):

    python scripts/build_vectorstore.py
    git add vectorstore/

Idempotente (upsert). El índice se persiste en vectorstore/ y se
VERSIONA en el repo junto a un manifest.json con los hashes de los
CSVs indexados — el build de Docker no regenera nada, solo copia el
índice y comprueba con check_vectorstore.py que no está desfasado.
"""

from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from app.rag.load_products import cargar_productos
from app.rag.manifest import escribir_manifiesto
from app.rag.vectorstore import indexar

if __name__ == "__main__":
    productos = cargar_productos()
    print(f"Indexando {len(productos)} productos en Chroma…")
    total = indexar(productos)
    escribir_manifiesto()
    print(f"Colección lista: {total} documentos. Manifiesto actualizado.")
