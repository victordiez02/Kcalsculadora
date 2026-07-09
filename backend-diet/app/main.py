"""FastAPI entrypoint del servicio de dietas."""

from __future__ import annotations

import os

from fastapi import FastAPI
from fastapi.exceptions import RequestValidationError
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from starlette.requests import Request

from . import constants as C
from .routers.diet import router as router_diet

app = FastAPI(
    title="Kcalsculadora Diet API",
    description=(
        "Genera un plan semanal de comidas con productos reales de supermercado "
        "a partir de las kcal/macros objetivo calculadas por la API principal. "
        "Único endpoint agregador: POST /diet/generate."
    ),
    version="0.1.0",
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
    """Catálogos para el wizard del frontend (supermercados, intolerancias,
    límites), para no duplicar constantes."""
    return {
        "supermercados": [{"id": sid, "label": s["label"]} for sid, s in C.SUPERMERCADOS.items()],
        "intolerancias": [{"id": iid, "label": i["label"]} for iid, i in C.INTOLERANCIAS.items()],
        "num_comidas": {"min": C.NUM_COMIDAS_MIN, "max": C.NUM_COMIDAS_MAX},
        "variedades": [
            {"id": vid, "label": v["label"], "menus": v["menus"]} for vid, v in C.VARIEDADES.items()
        ],
        "tolerancias": {"kcal_pct": C.TOLERANCIA_KCAL_PCT, "macro_pct": C.TOLERANCIA_MACRO_PCT},
    }


app.include_router(router_diet)
