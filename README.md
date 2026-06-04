<p align="center">
  <img src="frontend/public/logo.svg" alt="Kcalsculadora" width="100" />
</p>

<h1 align="center">Kcalsculadora</h1>

<p align="center">
  Calculadora nutricional para definición, recomposición y volumen.<br/>
  Calorías, macros, peso objetivo y duración de etapa a partir de
  composición corporal, nivel y objetivo.
</p>

<p align="center">
  <a href="https://kcalsculadora.vercel.app/"><img src="https://img.shields.io/badge/Abrir%20app-e46c44?style=for-the-badge" alt="Abrir app"></a>
  &nbsp;
  <a href="https://kcalsculadora.onrender.com/docs"><img src="https://img.shields.io/badge/API%20docs-1f2937?style=for-the-badge" alt="API docs"></a>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/React-18-61DAFB?logo=react&logoColor=white" />
  <img src="https://img.shields.io/badge/TypeScript-5-3178C6?logo=typescript&logoColor=white" />
  <img src="https://img.shields.io/badge/Vite-5-646CFF?logo=vite&logoColor=white" />
  <img src="https://img.shields.io/badge/Tailwind-3-38BDF8?logo=tailwindcss&logoColor=white" />
  <img src="https://img.shields.io/badge/FastAPI-0.115-009688?logo=fastapi&logoColor=white" />
  <img src="https://img.shields.io/badge/Python-3.10+-3776AB?logo=python&logoColor=white" />
</p>

---

## Qué es esto

Una App en React + TypeScript con una API en FastAPI. Mantiene la misma lógica de cálculo pero separada
en módulos atómicos y testeables, con un único endpoint agregador (`/plan`)
que es el que consume el front.

Calcula, a partir de peso, altura, edad, sexo, nivel y objetivo:

- % de grasa corporal (Deurenberg con variante para >36 años) si no se conoce.
- TMB con Mifflin–St Jeor y GET ajustado por actividad
  (multiplicadores de Mifflin para principiante/intermedio, Helms para avanzado).
- Calorías y macros según objetivo y agresividad del plan.
- Peso objetivo teniendo en cuenta la masa magra y la ganancia muscular
  esperable por nivel/edad.
- Semanas estimadas de etapa, con un ajuste suavizado cuando queda por debajo
  del mínimo recomendado.
- Avisos cuando la combinación no tiene mucho sentido (p. ej. volumen
  partiendo de grasa alta o definición con grasa muy baja).

Todas las fórmulas, multiplicadores y umbrales están en un solo sitio:
[`backend/app/constants.py`](backend/app/constants.py).

## Estructura

```
backend/app/
├── constants.py        # fórmulas, multiplicadores, umbrales
├── schemas.py          # modelos Pydantic
├── core/               # un módulo por cálculo + plan.py (orquestador)
├── routers/            # /calc/* atómicos y /plan agregador
└── main.py

frontend/src/
├── App.tsx             # formulario + resultados
├── components/         # tarjetas, banners, primitives shadcn/ui
├── api.ts · types.ts · constants.ts
```

La lista completa de endpoints con sus esquemas está en `/docs` (Swagger UI
que monta FastAPI).

## Desarrollo local

Necesitas Python 3.10+, Node 18+ y pnpm 9+ (`corepack enable`).

**Backend**

```bash
cd backend
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements-dev.txt
uvicorn app.main:app --reload --port 8000
```

**Frontend** (en otra terminal)

```bash
cd frontend
pnpm install
pnpm dev
```

El dev server de Vite hace proxy de `/api/*` al backend en `:8000`, así que
con los dos arrancados ya funciona end-to-end. La app queda en
<http://localhost:5173> y la API en <http://localhost:8000> (Swagger en
`/docs`).

## Build

```bash
cd frontend
pnpm build      # dist/
pnpm preview    # sirve dist/ en :4173
```

`frontend/dist/` se puede servir desde cualquier hosting estático. El backend
se levanta con `uvicorn` (o detrás de gunicorn con workers uvicorn). Hay que
ajustar el origen permitido por CORS en
[`backend/app/main.py`](backend/app/main.py) al dominio real del front.

## Lint

```bash
cd backend  && ruff check . && ruff format --check .
cd frontend && pnpm lint        # --max-warnings=0
```

## Licencia

[`LICENSE.txt`](LICENSE.txt).
