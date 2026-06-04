import { Card } from "@/components/ui/card";
import type { Macros } from "@/types";

interface Props {
  macros: Macros;
  calorias: number;
}

export function MacrosCard({ macros, calorias }: Props) {
  const kcalProt = macros.proteinas * 4;
  const kcalGra = macros.grasas * 9;
  const kcalCarb = macros.carbohidratos * 4;
  const total = Math.max(kcalProt + kcalGra + kcalCarb, 1);

  const items = [
    {
      label: "Proteínas",
      g: macros.proteinas,
      kcal: kcalProt,
      color: "#B05542",
      mark: "P",
    },
    {
      label: "Grasas",
      g: macros.grasas,
      kcal: kcalGra,
      color: "#D4A852",
      mark: "G",
    },
    {
      label: "Carbohidratos",
      g: macros.carbohidratos,
      kcal: kcalCarb,
      color: "#4C8059",
      mark: "C",
    },
  ];

  return (
    <Card className="p-5">
      <div className="flex items-baseline justify-between mb-4">
        <h3 className="font-display text-xl font-semibold">Macronutrientes</h3>
        <span className="font-mono text-xs uppercase tracking-wider text-ink/60">
          {Math.round(calorias)} kcal/día
        </span>
      </div>

      <div className="flex h-3 border-2 border-ink mb-4">
        {items.map((it) => (
          <div
            key={it.label}
            style={{
              width: `${(it.kcal / total) * 100}%`,
              background: it.color,
            }}
            className="border-r-2 border-ink last:border-r-0"
          />
        ))}
      </div>

      <ul className="space-y-3">
        {items.map((it) => (
          <li key={it.label} className="flex items-center gap-4">
            <div
              className="w-9 h-9 border-2 border-ink flex items-center justify-center font-display text-lg font-semibold text-white"
              style={{ background: it.color }}
            >
              {it.mark}
            </div>
            <div className="flex-1">
              <div className="font-mono text-xs uppercase tracking-wider text-ink/60">
                {it.label}
              </div>
              <div className="flex items-baseline gap-3">
                <span className="font-display text-2xl font-semibold">{it.g.toFixed(1)}</span>
                <span className="font-mono text-xs text-ink/60">g/día</span>
                <span className="font-mono text-xs text-ink/40">· {Math.round(it.kcal)} kcal</span>
              </div>
            </div>
            <div className="font-mono text-sm text-ink/70 tabular-nums">
              {Math.round((it.kcal / total) * 100)}%
            </div>
          </li>
        ))}
      </ul>
    </Card>
  );
}
