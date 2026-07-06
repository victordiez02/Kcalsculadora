"""Vector store de productos: Chroma local persistido en disco.

Cada producto se indexa con su nombre/marca/categoría como documento y
todos sus datos como metadata, de modo que el Retriever puede reconstruir
el producto completo sin releer los CSVs. Las intolerancias se guardan
como booleanos (`alergeno_<id>`) para poder excluirlas con filtros de
metadata, no con el LLM.
"""

from __future__ import annotations

import chromadb

from .. import constants as C
from ..schemas import Producto


def get_collection() -> chromadb.Collection:
    client = chromadb.PersistentClient(path=C.CHROMA_DIR)
    return client.get_or_create_collection(
        name=C.CHROMA_COLLECTION, metadata={"hnsw:space": "cosine"}
    )


def _documento(p: Producto) -> str:
    partes = [p.nombre, p.marca, C.CATEGORIA_LABELS.get(p.categoria, p.categoria)]
    return " · ".join(s for s in partes if s)


def _metadata(p: Producto) -> dict:
    meta: dict = {
        "code": p.code,
        "nombre": p.nombre,
        "marca": p.marca,
        "supermercado": p.supermercado,
        "categoria": p.categoria,
        "kcal_100g": p.kcal_100g,
        "proteinas_100g": p.proteinas_100g,
        "grasas_100g": p.grasas_100g,
        "carbohidratos_100g": p.carbohidratos_100g,
        "alergenos": "|".join(p.alergenos),
        "popularidad": p.popularidad,
    }
    for iid, intol in C.INTOLERANCIAS.items():
        meta[f"alergeno_{iid}"] = any(tag in p.alergenos for tag in intol["allergen_tags"])
    return meta


def producto_desde_metadata(meta: dict) -> Producto:
    return Producto(
        code=meta["code"],
        nombre=meta["nombre"],
        marca=meta["marca"],
        supermercado=meta["supermercado"],
        categoria=meta["categoria"],
        kcal_100g=meta["kcal_100g"],
        proteinas_100g=meta["proteinas_100g"],
        grasas_100g=meta["grasas_100g"],
        carbohidratos_100g=meta["carbohidratos_100g"],
        alergenos=[a for a in meta["alergenos"].split("|") if a],
        popularidad=int(meta.get("popularidad", 0)),
    )


def indexar(productos: list[Producto], batch: int = 500) -> int:
    """Indexa (upsert) los productos en la colección. Devuelve el total."""
    col = get_collection()
    for i in range(0, len(productos), batch):
        lote = productos[i : i + batch]
        col.upsert(
            ids=[f"{p.supermercado}:{p.code}" for p in lote],
            documents=[_documento(p) for p in lote],
            metadatas=[_metadata(p) for p in lote],
        )
    return col.count()


def buscar(
    query: str,
    supermercado: str,
    categorias: list[str] | None = None,
    intolerancias: list[str] | None = None,
    n: int = 10,
) -> list[Producto]:
    """Búsqueda semántica con filtros duros de metadata (supermercado,
    categorías permitidas y exclusión garantizada de alérgenos)."""
    col = get_collection()
    clausulas: list[dict] = [{"supermercado": {"$eq": supermercado}}]
    if categorias:
        clausulas.append({"categoria": {"$in": categorias}})
    for iid in intolerancias or []:
        clausulas.append({f"alergeno_{iid}": {"$eq": False}})
    where = clausulas[0] if len(clausulas) == 1 else {"$and": clausulas}

    res = col.query(query_texts=[query], n_results=n, where=where)
    return [producto_desde_metadata(m) for m in res["metadatas"][0]]
