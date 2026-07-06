"""Avisos contextuales según composición corporal y objetivo."""

from __future__ import annotations

from .. import constants as C
from ..schemas import Aviso, Objetivo, Sexo


def avisos(objetivo: Objetivo, sexo: Sexo, grasa_pct: float) -> list[Aviso]:
    out: list[Aviso] = []

    if objetivo == "mant":
        out.append(
            Aviso(
                nivel="info",
                mensaje=(
                    "Recomposición: progresa despacio. Con ~1.6 g/kg de proteína al día "
                    "ya cubres la síntesis muscular; prioriza los carbohidratos para "
                    "tener energía y rendir en cada entreno."
                ),
            )
        )

    if objetivo == "vol" and grasa_pct > C.GRASA_VOL_LIMITE_ADVERTENCIA[sexo]:
        out.append(
            Aviso(
                nivel="warning",
                mensaje=(
                    "Tu % de grasa es alto para un volumen. "
                    "Considera una fase de definición primero."
                ),
            )
        )

    if objetivo == "def" and grasa_pct < C.GRASA_DEF_LIMITE_ADVERTENCIA[sexo]:
        out.append(
            Aviso(
                nivel="danger",
                mensaje=(
                    "Definir con un % de grasa tan bajo puede ser arriesgado. "
                    "Plantea un volumen controlado."
                ),
            )
        )

    if grasa_pct >= C.GRASA_RECOMENDAR_DEF[sexo]:
        out.append(
            Aviso(
                nivel="info",
                mensaje=(
                    "Tu % de grasa sugiere una fase de definición antes de plantear un volumen."
                ),
            )
        )
    elif grasa_pct <= C.GRASA_RECOMENDAR_VOL[sexo]:
        out.append(
            Aviso(
                nivel="info",
                mensaje=("Tu % de grasa es bajo: una fase de volumen sería buena candidata."),
            )
        )

    return out
