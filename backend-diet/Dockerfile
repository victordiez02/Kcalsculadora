FROM python:3.11.9-slim

ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Dependencias nativas de WeasyPrint (Pango) y fuentes para el PDF
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpango-1.0-0 libpangocairo-1.0-0 libgdk-pixbuf-2.0-0 fonts-dejavu-core \
    && rm -rf /var/lib/apt/lists/*

# Instalar uv (instalador de paquetes ultra rápido, reemplaza a pip)
COPY --from=ghcr.io/astral-sh/uv:latest /uv /usr/local/bin/uv

WORKDIR /app

COPY requirements.txt .

# uv resuelve e instala dependencias mucho más rápido que pip
RUN uv pip install --system --no-cache -r requirements.txt

# Modelo ONNX de MiniLM: en runtime Chroma embebe las queries del Retriever,
# así que se pre-descarga aquí (capa cacheada) en vez de en el primer arranque.
RUN python -c "from chromadb.utils.embedding_functions import ONNXMiniLM_L6_V2; ONNXMiniLM_L6_V2()(['warmup'])"

# El índice Chroma NO se genera en el build: va pre-construido y versionado
# en el repo (vectorstore/) y entra en la imagen con este COPY. Solo se
# regenera en local (scripts/build_vectorstore.py) cuando cambian los CSVs.
COPY . .

# Falla el build si los CSVs cambiaron sin regenerar el índice (solo compara
# hashes contra vectorstore/manifest.json, no calcula embeddings).
RUN python scripts/check_vectorstore.py

CMD ["sh", "-c", "uvicorn app.main:app --host 0.0.0.0 --port ${PORT}"]
