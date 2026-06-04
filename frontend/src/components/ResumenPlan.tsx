import { Card } from "@/components/ui/card";
import type { CalculoOutput } from "@/types";

interface Props {
  data: CalculoOutput;
  pesoActual: number;
}

export function ResumenPlan({ data, pesoActual }: Props) {
  const { ajuste_recomendado: ar } = data;
  return (
    <Card className="p-6 relative">
      <span className="absolute -top-3 left-5 bg-sand px-3 font-mono text-[10px] uppercase tracking-[0.25em] text-ink/60">
        resumen del plan
      </span>

      <div className="grid grid-cols-2 gap-6 mb-5">
        <Stat
          label="Peso objetivo"
          value={`${data.peso_objetivo} kg`}
          hint={`actual: ${pesoActual} kg`}
        />
        <Stat
          label="Duración estimada"
          value={data.semanas_necesarias != null ? `${data.semanas_necesarias} sem` : "—"}
          hint={
            data.semanas_minimas > 0
              ? `mínimo recomendado: ${data.semanas_minimas} sem`
              : "sin plazo"
          }
        />
        <Stat
          label="Ajuste calórico"
          value={`${data.tipo_ajuste === "reducir" ? "−" : data.tipo_ajuste === "aumentar" ? "+" : "±"}${data.ajuste_calorico} kcal`}
          hint="kcal/día respecto al GET"
        />
        <Stat
          label="Ganancia muscular"
          value={`${data.ganancia_muscular_kg} kg`}
          hint="estimada en el periodo"
        />
        {data.perdida_grasa_kg > 0 && (
          <Stat
            label="Pérdida de grasa"
            value={`${data.perdida_grasa_kg} kg`}
            hint="estimada en el periodo"
          />
        )}
      </div>

      {data.semanas_necesarias != null && data.semanas_necesarias > 0 && (
        <p className="text-sm leading-relaxed text-ink/80 border-t-2 border-ink/10 pt-4">
          Se planifica un cambio de{" "}
          <strong>{Math.abs(pesoActual - data.peso_objetivo).toFixed(1)} kg</strong> en{" "}
          <strong>{data.semanas_necesarias} semanas</strong> (
          {Math.round((Math.abs(pesoActual - data.peso_objetivo) * 1000) / data.semanas_necesarias)}{" "}
          g/semana), sin sobrepasar el <strong>{data.grasa_objetivo}% de grasa corporal</strong>.
        </p>
      )}

      {ar && (
        <div className="mt-5 border-2 border-ochre bg-ochre/10 p-4">
          <h4 className="font-display text-lg font-semibold mb-1">Ajuste recomendado</h4>
          <p className="text-sm mb-3">{ar.motivo}</p>
          <div className="grid grid-cols-2 sm:grid-cols-4 gap-3 text-sm font-mono">
            <div>
              <div className="text-[10px] uppercase tracking-wider text-ink/60">Calorías</div>
              <div className="text-base font-semibold">{ar.calorias} kcal/día</div>
            </div>
            <div>
              <div className="text-[10px] uppercase tracking-wider text-ink/60">Δ vs GET</div>
              <div className="text-base font-semibold">±{ar.delta_kcal_dia} kcal</div>
            </div>
            <div>
              <div className="text-[10px] uppercase tracking-wider text-ink/60">Mín. semanas</div>
              <div className="text-base font-semibold">{ar.semanas_minimas}</div>
            </div>
            <div>
              <div className="text-[10px] uppercase tracking-wider text-ink/60">Ritmo</div>
              <div className="text-base font-semibold">{ar.gramos_por_semana} g/sem</div>
            </div>
          </div>
          <div className="mt-3 grid grid-cols-3 gap-3 text-xs font-mono">
            <div>P · {ar.macros.proteinas} g</div>
            <div>G · {ar.macros.grasas} g</div>
            <div>C · {ar.macros.carbohidratos} g</div>
          </div>
        </div>
      )}
    </Card>
  );
}

function Stat({ label, value, hint }: { label: string; value: string; hint?: string }) {
  return (
    <div>
      <div className="font-mono text-[10px] uppercase tracking-[0.2em] text-ink/60 mb-1">
        {label}
      </div>
      <div className="font-display font-semibold text-ink leading-none tabular-nums text-3xl">
        {value}
      </div>
      {hint && <div className="font-mono text-xs text-ink/50 mt-1">{hint}</div>}
    </div>
  );
}
