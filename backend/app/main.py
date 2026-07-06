"""FastAPI entrypoint."""

from __future__ import annotations
import os
from fastapi import FastAPI
from fastapi.exceptions import RequestValidationError
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from starlette.requests import Request

from . import constants as C
from .routers.calculos import router as router_calculos
from .routers.plan import router as router_plan

app = FastAPI(
    title="Kcalsculadora API",
    description=(
        "API de cálculo fitness (TMB, GET, macros, peso objetivo). "
        "Cada cálculo tiene un endpoint atómico bajo /calc/* y existe un "
        "agregador /plan que compone el resultado completo."
    ),
    version="2.0.0",
)

origins_env = os.environ.get("ALLOWED_ORIGINS")
if not origins_env:
    raise KeyError("Falta la variable de entorno 'ALLOWED_ORIGINS' requerida para configurar CORS.")

app.add_middleware(
    CORSMiddleware,
    allow_origins=[o.strip() for o in origins_env.split(",")],
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.exception_handler(RequestValidationError)
async def _validation_handler(_: Request, exc: RequestValidationError) -> JSONResponse:
    """Devuelve los errores de validación con un mensaje legible."""
    mensajes = [
        {"campo": ".".join(str(p) for p in e["loc"][1:]), "error": e["msg"]} for e in exc.errors()
    ]
    return JSONResponse(status_code=422, content={"detalle": mensajes})


@app.get("/health", tags=["meta"])
def health() -> dict[str, str]:
    return {"status": "ok"}


@app.get("/config", tags=["meta"])
def config() -> dict:
    """Catálogos parametrizables (factores y límites) para que el frontend
    no duplique constantes numéricas."""
    return {
        "actividad_mifflin": C.ACTIVIDAD_MIFFLIN,
        "actividad_helms": C.ACTIVIDAD_HELMS,
        "agresividad": C.AGRESIVIDAD,
        "limites": C.LIMITES,
    }


app.include_router(router_calculos)
app.include_router(router_plan)
