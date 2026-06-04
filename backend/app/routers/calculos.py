"""Endpoints atómicos: un router por cálculo."""

from __future__ import annotations

from typing import Literal

from fastapi import APIRouter

from .. import constants as C
from ..core.avisos import avisos as fn_avisos
from ..core.calorias import calorias_recomendadas
from ..core.ganancia import duracion_minima, ganancia_muscular_semanal
from ..core.get import calcular_get
from ..core.grasa import estimar_grasa, masa_magra
from ..core.imc import calcular_imc, categoria_imc
from ..core.macros import calcular_macros
from ..core.peso_objetivo import calcular_peso_objetivo
from ..core.semanas import calcular_semanas
from ..core.tmb import calcular_tmb
from ..schemas import (
    Agresividad,
    Aviso,
    CaloriasInput,
    CaloriasOutput,
    GetInput,
    GetOutput,
    GrasaInput,
    GrasaOutput,
    ImcInput,
    ImcOutput,
    Macros,
    MacrosInput,
    Nivel,
    Objetivo,
    PesoObjetivoInput,
    PesoObjetivoOutput,
    SemanasInput,
    SemanasOutput,
    Sexo,
    TmbInput,
    TmbOutput,
)

router = APIRouter(prefix="/calc", tags=["cálculos atómicos"])

TipoAjuste = Literal["reducir", "aumentar", "mantener"]


def _tipo_ajuste(objetivo: Objetivo) -> TipoAjuste:
    if objetivo == "def":
        return "reducir"
    if objetivo == "vol":
        return "aumentar"
    return "mantener"


@router.post("/grasa", response_model=GrasaOutput)
def grasa(data: GrasaInput) -> GrasaOutput:
    g = estimar_grasa(data.peso, data.altura, data.edad, data.sexo, data.nivel)
    return GrasaOutput(grasa_estimada=g, masa_magra=round(masa_magra(data.peso, g), 2))


@router.post("/tmb", response_model=TmbOutput)
def tmb(data: TmbInput) -> TmbOutput:
    return TmbOutput(tmb=round(calcular_tmb(data.peso, data.altura, data.edad, data.sexo), 1))


@router.post("/get", response_model=GetOutput)
def get_endpoint(data: GetInput) -> GetOutput:
    return GetOutput(get=round(calcular_get(data.tmb, data.actividad), 1))


@router.post("/imc", response_model=ImcOutput)
def imc(data: ImcInput) -> ImcOutput:
    valor = calcular_imc(data.peso, data.altura)
    return ImcOutput(imc=round(valor, 2), categoria=categoria_imc(valor))


@router.post("/calorias", response_model=CaloriasOutput)
def calorias(data: CaloriasInput) -> CaloriasOutput:
    kcal = calorias_recomendadas(
        data.objetivo, data.nivel, data.agresividad, data.get, enfoque_recomp=data.enfoque_recomp
    )
    return CaloriasOutput(
        calorias_recomendadas=round(kcal, 0),
        ajuste_calorico=round(abs(kcal - data.get), 0),
        tipo_ajuste=_tipo_ajuste(data.objetivo),
    )


@router.post("/macros", response_model=Macros)
def macros(data: MacrosInput) -> Macros:
    return calcular_macros(
        data.peso, data.peso_objetivo, data.calorias, data.objetivo, data.sexo
    )


@router.post("/peso-objetivo", response_model=PesoObjetivoOutput)
def peso_objetivo(data: PesoObjetivoInput) -> PesoObjetivoOutput:
    po = calcular_peso_objetivo(
        data.peso, data.masa_magra, data.incremento_masa_magra,
        data.objetivo, data.sexo, data.grasa,
    )
    return PesoObjetivoOutput(
        peso_objetivo=round(po, 2),
        grasa_objetivo=C.GRASA_OBJ_POR_OBJETIVO[data.objetivo][data.sexo],
    )


@router.post("/semanas", response_model=SemanasOutput)
def semanas(data: SemanasInput) -> SemanasOutput:
    s = calcular_semanas(
        data.objetivo, data.agresividad, data.peso, data.peso_objetivo,
        data.calorias_recomendadas, data.get,
    )
    return SemanasOutput(semanas_necesarias=round(s, 1) if s is not None else None)


@router.get("/duracion-minima")
def duracion_minima_endpoint(
    objetivo: Objetivo, nivel: Nivel, grasa: float, sexo: Sexo
) -> dict[str, int]:
    return {"semanas_minimas": duracion_minima(objetivo, nivel, grasa, sexo)}


@router.get("/ganancia-muscular-semanal")
def ganancia_endpoint(
    objetivo: Objetivo,
    nivel: Nivel,
    agresividad: Agresividad,
    actividad: float,
    sexo: Sexo,
    edad: int,
) -> dict[str, float]:
    return {
        "kg_por_semana": round(
            ganancia_muscular_semanal(objetivo, nivel, agresividad, actividad, sexo, edad), 4
        )
    }


@router.get("/avisos")
def avisos_endpoint(objetivo: Objetivo, sexo: Sexo, grasa: float) -> dict[str, list[Aviso]]:
    return {"avisos": fn_avisos(objetivo, sexo, grasa)}
