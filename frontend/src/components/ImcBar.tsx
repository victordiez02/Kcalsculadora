import { Tooltip, TooltipContent, TooltipTrigger } from "@/components/ui/tooltip";
import { Card } from "@/components/ui/card";
import { IMC_BANDAS } from "@/constants";

interface Props {
  imc: number;
  imcObjetivo?: number | null;
}

const SCALE_MAX = 45;

function clamp(v: number) {
  return Math.min(Math.max(v, 0), SCALE_MAX);
}

export function ImcBar({ imc, imcObjetivo }: Props) {
  const pctActual = (clamp(imc) / SCALE_MAX) * 100;
  const showObjetivo =
    imcObjetivo != null && Number.isFinite(imcObjetivo) && Math.abs(imcObjetivo - imc) >= 0.05;
  const pctObjetivo = showObjetivo ? (clamp(imcObjetivo) / SCALE_MAX) * 100 : 0;

  return (
    <Card className="p-5">
      <div className="flex items-baseline justify-between mb-3">
        <h3 className="font-display text-xl font-semibold">Índice de masa corporal</h3>
        <div className="font-mono text-sm uppercase tracking-wider text-ink/60">
          IMC = <span className="text-ink font-medium">{imc.toFixed(2)}</span>
        </div>
      </div>

      <div className="relative h-12 border-2 border-ink flex">
        {IMC_BANDAS.map((b, i) => {
          const prev = i === 0 ? 0 : IMC_BANDAS[i - 1].tope;
          const width = ((b.tope - prev) / SCALE_MAX) * 100;
          return (
            <div
              key={b.label}
              className="relative h-full border-r border-ink/40 last:border-r-0 flex items-center justify-center"
              style={{ width: `${width}%`, background: b.color }}
            >
              <span
                className="text-[10px] font-mono uppercase tracking-wider px-1 text-center leading-tight"
                style={{ color: "rgb(26 24 21 / 0.8)" }}
              >
                {b.label}
              </span>
            </div>
          );
        })}

        {showObjetivo && (
          <Tooltip>
            <TooltipTrigger asChild>
              <div
                className="absolute top-[-8px] bottom-[-8px] w-[3px] bg-ember cursor-help"
                style={{ left: `calc(${pctObjetivo}% - 1.5px)` }}
                aria-label={`IMC objetivo ${imcObjetivo!.toFixed(2)}`}
              >
                <div className="absolute -bottom-2 left-1/2 -translate-x-1/2 w-3 h-3 rotate-45 bg-ember" />
              </div>
            </TooltipTrigger>
            <TooltipContent side="bottom">Objetivo · IMC {imcObjetivo!.toFixed(2)}</TooltipContent>
          </Tooltip>
        )}

        <Tooltip>
          <TooltipTrigger asChild>
            <div
              className="absolute top-[-8px] bottom-[-8px] w-[3px] bg-ink ring-1 ring-sand cursor-help"
              style={{ left: `calc(${pctActual}% - 1.5px)` }}
              aria-label={`IMC actual ${imc.toFixed(2)}`}
            >
              <div className="absolute -top-2 left-1/2 -translate-x-1/2 w-3 h-3 rotate-45 bg-ink ring-1 ring-sand" />
            </div>
          </TooltipTrigger>
          <TooltipContent side="top">Actual · IMC {imc.toFixed(2)}</TooltipContent>
        </Tooltip>
      </div>

      <div className="mt-2 flex justify-between font-mono text-[10px] text-ink/50">
        <span>0</span>
        <span>18.5</span>
        <span>25</span>
        <span>30</span>
        <span>35</span>
        <span>40</span>
        <span>45+</span>
      </div>

      {showObjetivo && (
        <div className="mt-3 flex items-center gap-4 font-mono text-[10px] uppercase tracking-wider text-ink/60">
          <span className="flex items-center gap-1.5">
            <span className="inline-block w-3 h-[3px] bg-ink" /> actual
          </span>
          <span className="flex items-center gap-1.5">
            <span className="inline-block w-3 h-[3px] bg-ember" /> objetivo
          </span>
        </div>
      )}
    </Card>
  );
}
