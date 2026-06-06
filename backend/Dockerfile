FROM python:3.11.9-slim

ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

WORKDIR /app

COPY requirements.txt .

RUN pip install --no-cache-dir --upgrade -r requirements.txt

# Copiamos todo el contenido de la carpeta backend al contenedor
COPY . .

# Comando corregido para tu estructura: entra a la carpeta 'app', busca 'main.py' y arranca 'app'
CMD ["sh", "-c", "uvicorn app.main:app --host 0.0.0.0 --port ${PORT}"]