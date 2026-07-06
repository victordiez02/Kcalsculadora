<p align="center">
  <img src="frontend/public/logo.svg" alt="Kcalsculadora" width="100" />
</p>

<h1 align="center">Kcalsculadora</h1>

<p align="center">
  Nutrition planning platform for cutting, recomposition and bulking phases.<br/>
  Calculates calories, macronutrients, target weight and estimated phase duration — and turns
  those numbers into a real daily meal plan built from actual supermarket products, exportable to PDF.
</p>

<p align="center">
  <a href="https://kcalsculadora.vercel.app/"><img src="https://img.shields.io/badge/Open%20App-e46c44?style=for-the-badge" alt="Open App"></a>
  &nbsp;
  <a href="https://kcalsculadora-backend-762078704585.europe-west1.run.app/docs"><img src="https://img.shields.io/badge/API%20Docs-1f2937?style=for-the-badge" alt="API Docs"></a>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/React-18-61DAFB?logo=react&logoColor=white" />
  <img src="https://img.shields.io/badge/TypeScript-5-3178C6?logo=typescript&logoColor=white" />
  <img src="https://img.shields.io/badge/Vite-5-646CFF?logo=vite&logoColor=white" />
  <img src="https://img.shields.io/badge/Tailwind-3-38BDF8?logo=tailwindcss&logoColor=white" />
  <img src="https://img.shields.io/badge/FastAPI-Python-009688?logo=fastapi&logoColor=white" />
  <img src="https://img.shields.io/badge/LangGraph-Agents-1C3C3C?logo=langchain&logoColor=white" />
  <img src="https://img.shields.io/badge/Python-3.11-3776AB?logo=python&logoColor=white" />
</p>

---

## Overview

Kcalsculadora is a monorepo with **one frontend and two independent backend services**:

| Service | Role |
|---------|------|
| `frontend/` | Single React app. Consumes both APIs. |
| `backend-kcal/` | Nutrition math: body fat %, BMR, TDEE, calorie/macro targets, target weight and phase duration. Single aggregation endpoint: `POST /plan`. |
| `backend-diet/` | **AI meal-plan generator.** Takes the targets computed by `backend-kcal` and builds a real daily meal plan (breakfast/lunch/snack/dinner) using actual products from a chosen Spanish supermarket, then renders it as a PDF. Single aggregation endpoint: `POST /diet/generate`. |

### How the diet generator works

1. The user completes a progressive wizard (supermarket, meals per day, intolerances,
   foods to avoid, favourites, variety preference).
2. A **LangGraph** state machine orchestrates the generation:
   - **Planner** (LLM, strict JSON output) splits daily kcal/macros across meals — the split is
     re-normalized in Python so totals always match the target exactly.
   - **Retriever** (pure RAG, no LLM) queries a local **Chroma** vector store of real products
     (from **Open Food Facts**, filtered by supermarket) with hard metadata filters for
     supermarket, food category and declared allergens.
   - **Composer** (LLM) picks concrete products and gram amounts from the candidates. It can only
     reference products by code — kcal/macros are always computed in Python from the dataset,
     never invented by the model.
   - **Validator** (deterministic Python) compares the plan against the target with configured
     tolerances (±3% kcal, ±7% per macro). Out of tolerance → retry Composer (or Planner if the
     deviation is large) with explicit numeric feedback; retries exhausted → return the best plan
     found, flagged as approximate. It never fails silently.
3. The final plan is rendered to PDF with **WeasyPrint** using a fixed Jinja2 template that
   mirrors the app's aesthetic.

The product dataset is built offline from the Open Food Facts Parquet dump
(`backend-diet/scripts/build_dataset.py`), filtered by the `stores` field (Mercadona, Carrefour,
Lidl, Dia), sanity-checked (kcal must be consistent with declared macros) and committed as static
CSVs. No external calls at runtime besides the LLM.

---

## Architecture

```text
backend-kcal/app/           # existing calculation service (untouched)
├── constants.py            # formulas, multipliers and thresholds
├── schemas.py              # Pydantic models
├── core/                   # calculation modules and plan orchestrator
└── routers/                # atomic endpoints + aggregated /plan

backend-diet/app/           # AI meal-plan service
├── constants.py            # tolerances, retries, catalogs, dataset mappings
├── schemas.py              # request/response + LLM structured outputs
├── graph/                  # LangGraph: state, nodes, build (conditional edges)
├── rag/                    # Open Food Facts dataset + Chroma vector store
├── pdf/                    # fixed Jinja2 template + WeasyPrint rendering
└── routers/diet.py         # single aggregated POST /diet/generate

frontend/src/
├── api/                    # kcalApi.ts (VITE_API_URL) + dietApi.ts (VITE_DIET_API_URL)
├── components/diet/        # wizard dialog, plan view, PDF download
└── App.tsx                 # form, results and diet integration
```

Both services expose Swagger UI at `/docs`.

---

## Local Development

Requirements: Python 3.11+, Node.js 18+, pnpm 9+, an OpenAI API key (only for `backend-diet`).

### backend-kcal (port 8000)

```powershell
cd backend-kcal
python -m venv .venv; .venv\Scripts\Activate.ps1
pip install -r requirements.txt
$env:ALLOWED_ORIGINS = "http://localhost:5173"
uvicorn app.main:app --reload --port 8000
```

### backend-diet (port 8001)

```powershell
cd backend-diet
python -m venv .venv; .venv\Scripts\Activate.ps1
pip install -r requirements-dev.txt
# fill OPENAI_API_KEY in backend-diet/.env (see .env.example)
python scripts/build_vectorstore.py   # first time only: builds the Chroma index
uvicorn app.main:app --reload --port 8001
```

> **Note (Windows):** PDF generation needs WeasyPrint's native Pango libraries, which are not
> available on Windows by default. The service still works and returns the plan without the PDF;
> inside Docker (Linux) the PDF works out of the box.

To rebuild the product dataset from a fresh Open Food Facts dump:

```powershell
python scripts/build_dataset.py          # streams the dump from Hugging Face
python scripts/build_dataset.py --parquet C:\path\to\food.parquet   # local copy (faster)
python scripts/build_vectorstore.py
```

### frontend (port 5173)

```powershell
cd frontend
pnpm install
pnpm dev
```

The Vite dev server proxies `/api` → `localhost:8000` and `/diet-api` → `localhost:8001`, so no
env vars are needed locally.

### Everything at once (Docker)

```bash
docker compose up --build
```

Brings up the three services (frontend on `:5173`, backend-kcal on `:8000`, backend-diet on
`:8001`). Requires `backend-diet/.env` with `OPENAI_API_KEY` filled.

---

## Environment variables

| Variable | Service | Purpose |
|----------|---------|---------|
| `ALLOWED_ORIGINS` | both backends | comma-separated CORS origins |
| `OPENAI_API_KEY` | backend-diet | LLM for Planner/Composer |
| `OPENAI_MODEL` | backend-diet | optional model override (default `gpt-4o-mini`) |
| `CHROMA_DIR` | backend-diet | optional vector store location override |
| `VITE_API_URL` | frontend | backend-kcal base URL (prod) |
| `VITE_DIET_API_URL` | frontend | backend-diet base URL (prod) |

---

## Code Quality

```bash
cd backend-kcal && ruff check . && ruff format --check .
cd backend-diet && ruff check . && ruff format --check . && pytest
cd frontend     && pnpm lint && pnpm build
```

---

## Tech Stack

| Layer | Technologies |
|---------|---------|
| Frontend | React 18, TypeScript, Vite, Tailwind CSS, Radix UI, framer-motion |
| Backend (kcal) | Python 3.11, FastAPI, Pydantic v2 |
| Backend (diet) | Python 3.11, FastAPI, LangGraph, LangChain (OpenAI), Chroma, WeasyPrint |
| Data | Open Food Facts (static dataset, filtered by supermarket) |
| Deployment | Vercel (frontend) + Cloud Run (backends, Docker) |

---

## License

See `LICENSE.txt`.
