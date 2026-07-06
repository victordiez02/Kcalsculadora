"""Indexa el dataset de productos en el vector store Chroma local.

Uso (tras generar los CSVs con build_dataset.py):

    python scripts/build_vectorstore.py

Idempotente (upsert). El índice se persiste en app/rag/chroma/ (no se
versiona: se reconstruye desde los CSVs, también al construir la imagen
Docker).
"""

from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from app.rag.load_products import cargar_productos
from app.rag.vectorstore import indexar

if __name__ == "__main__":
    productos = cargar_productos()
    print(f"Indexando {len(productos)} productos en Chroma…")
    total = indexar(productos)
    print(f"Colección lista: {total} documentos.")
