"""Manifiesto del vector store: hashes de los CSVs con los que se indexó.

El índice Chroma va versionado en el repo (vectorstore/) junto a un
manifest.json con el sha256 de cada CSV de datos. Así se puede detectar
sin recalcular embeddings si el índice está desfasado respecto a los
CSVs (scripts/check_vectorstore.py, ejecutado en el build de Docker).
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

from .. import constants as C
from .load_products import DATA_DIR


def _sha256(ruta: Path) -> str:
    h = hashlib.sha256()
    with ruta.open("rb") as f:
        for bloque in iter(lambda: f.read(65536), b""):
            h.update(bloque)
    return h.hexdigest()


def hashes_actuales() -> dict[str, str]:
    """sha256 de cada CSV del dataset, por supermercado."""
    return {sid: _sha256(DATA_DIR / f"{sid}.csv") for sid in C.SUPERMERCADOS}


def escribir_manifiesto() -> None:
    """Guarda los hashes de los CSVs recién indexados junto al índice."""
    Path(C.VECTORSTORE_MANIFEST).write_text(
        json.dumps({"csv_sha256": hashes_actuales()}, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


def csvs_desfasados() -> list[str]:
    """Supermercados cuyo CSV no coincide con el manifiesto del índice.

    Devuelve todos los ids si no existe manifiesto o índice (nunca indexado).
    """
    if not Path(C.VECTORSTORE_MANIFEST).exists() or not Path(C.CHROMA_DIR).exists():
        return list(C.SUPERMERCADOS)
    manifiesto = json.loads(Path(C.VECTORSTORE_MANIFEST).read_text(encoding="utf-8"))
    indexados = manifiesto.get("csv_sha256", {})
    return [sid for sid, h in hashes_actuales().items() if indexados.get(sid) != h]
