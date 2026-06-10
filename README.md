<p align="center">
  <img src="frontend/public/logo.svg" alt="Kcalsculadora" width="100" />
</p>

<h1 align="center">Kcalsculadora</h1>

<p align="center">
  Nutrition planning platform for cutting, recomposition and bulking phases.<br/>
  Calculates calories, macronutrients, target weight and estimated phase duration based on body composition, training level and fitness goals.
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
  <img src="https://img.shields.io/badge/Python-3.10+-3776AB?logo=python&logoColor=white" />
</p>

---

## Overview

Kcalsculadora is a full-stack web application built with React, TypeScript and FastAPI.

The project translates established nutrition and body composition formulas into modular and testable backend components exposed through a single aggregation endpoint (`/plan`). The frontend consumes this endpoint and presents a complete nutrition plan based on the user's profile and goals.

Given a user's weight, height, age, sex, training level and objective, the application calculates:

- Body fat percentage using the Deurenberg equation (including an adjusted variant for users over 36 years old).
- Basal Metabolic Rate (Mifflin-St Jeor) and Total Daily Energy Expenditure adjusted by activity level.
- Daily calorie and macronutrient targets.
- Target body weight based on lean body mass and expected muscle gain potential.
- Estimated duration of the planned phase.
- Contextual warnings when the selected objective may not be appropriate for the user's body composition.

All formulas, constants and thresholds are centralized in:

```text
backend/app/constants.py
```

---

## Key Features

- Full-stack architecture using React, TypeScript and FastAPI.
- Modular backend design with atomic calculation services.
- Centralized business rules and nutrition formulas.
- Single aggregation endpoint for complete nutrition planning.
- Input validation through Pydantic schemas.
- Interactive frontend for data entry and result visualization.

---

## Architecture

```text
backend/app/
├── constants.py        # formulas, multipliers and thresholds
├── schemas.py          # Pydantic models
├── core/               # calculation modules and plan orchestrator
├── routers/            # atomic endpoints and aggregated /plan endpoint
└── main.py

frontend/src/
├── App.tsx             # form and results
├── components/         # UI components
├── api.ts
├── types.ts
└── constants.ts
```

FastAPI automatically exposes interactive API documentation through Swagger UI at:

```text
/docs
```

---

## Local Development

Requirements:

- Python 3.10+
- Node.js 18+
- pnpm 9+

### Backend

```bash
cd backend
python3 -m venv .venv
.venv\Scripts\Activate.ps1
pip install -r requirements-dev.txt
uvicorn app.main:app --reload --port 8000
```

### Frontend

```bash
cd frontend
pnpm install
pnpm dev
```

The Vite development server proxies API requests to the FastAPI backend, allowing the application to run end-to-end in a local environment.

---

## Build

```bash
cd frontend
pnpm build
pnpm preview
```

The frontend generates a static build that can be deployed on any static hosting provider. The backend can be deployed using Uvicorn or behind Gunicorn with Uvicorn workers.

---

## Code Quality

```bash
cd backend  && ruff check . && ruff format --check .
cd frontend && pnpm lint
```

---

## Tech Stack

| Layer | Technologies |
|---------|---------|
| Frontend | React 18, TypeScript, Vite, Tailwind CSS |
| Backend | Python 3.11, FastAPI, Pydantic |
| Validation | Pydantic |
| Deployment | Static Hosting + FastAPI Backend |

---

## License

See `LICENSE.txt`.
