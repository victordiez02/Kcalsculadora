"""Comprueba que el índice Chroma versionado está al día con los CSVs.

Solo compara hashes (rápido, sin embeddings ni Chroma). Se ejecuta en el
build de Docker: si algún CSV cambió sin regenerar el índice, el build
falla con instrucciones en vez de hornear un índice desfasado.

    python scripts/check_vectorstore.py
"""

from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from app.rag.manifest import csvs_desfasados

if __name__ == "__main__":
    desfasados = csvs_desfasados()
    if desfasados:
        print(
            "El índice Chroma (vectorstore/) está desfasado respecto a los CSVs: "
            f"{', '.join(desfasados)}.\n"
            "Regenéralo y versiona el resultado:\n"
            "    python scripts/build_vectorstore.py\n"
            "    git add vectorstore/",
            file=sys.stderr,
        )
        sys.exit(1)
    print("Vector store al día con los CSVs.")
