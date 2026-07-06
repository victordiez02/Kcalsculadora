import { motion } from "framer-motion";
import { AlertTriangle, Download, RefreshCcw, UtensilsCrossed } from "lucide-react";

import { descargarPdf } from "@/api/dietApi";
import { SUPERMERCADOS } from "@/constants";
import type { ComidaPlan, DietOutput, Macros, Supermercado } from "@/types";
import { Button } from "@/components/ui/button";
import { Card } from "@/components/ui/card";
import { cn } from "@/lib/utils";

export function PlanComidas({
  plan,
  supermercado,
  kcalObjetivo,
  macrosObjetivo,
  onRegenerar,
}: {
  plan: DietOutput;
  supermercado: Supermercado | null;
  kcalObjetivo: number;
  macrosObjetivo: Macros;
  onRegenerar: () => void;
}) {
  const superLabel = SUPERMERCADOS.find((s) => s.id === supermercado)?.label;
  return (
    <Card className="p-4 sm:p-6 scroll-mt-6" id="plan-comidas">
      <div className="flex items-start justify-between gap-3 flex-wrap mb-4">
        <div>
          <h2 className="font-display text-xl sm:text-2xl font-semibold flex items-center gap-2">
            <UtensilsCrossed size={20} className="text-ember" /> Tu plan de comidas
          </h2>
          <p className="font-mono text-[11px] uppercase tracking-wider text-ink/50 mt-1">
            {superLabel ? `productos de ${superLabel}` : "productos reales"} ·{" "}
            {plan.intentos_usados} intento{plan.intentos_usados !== 1 ? "s" : ""}
          </p>
        </div>
        <div className="flex gap-2">
          {plan.pdf_base64 && (
            <Button size="sm" type="button" onClick={() => descargarPdf(plan.pdf_base64!)}>
              <Download size={15} /> PDF
            </Button>
          )}
          <Button variant="ghost" size="sm" type="button" onClick={onRegenerar}>
            <RefreshCcw size={14} />
            <span className="hidden sm:inline">Regenerar</span>
          </Button>
        </div>
      </div>

      {plan.aproximado && (
        <div className="mb-4 border-2 border-ochre bg-ochre/15 px-3.5 py-2.5 text-sm flex items-start gap-2">
          <AlertTriangle size={16} className="text-ochre flex-shrink-0 mt-0.5" />
          <span>
            No se pudo clavar tu objetivo con estos productos: este es el{" "}
            <strong>mejor plan encontrado</strong>. Revisa la desviación abajo o regenera con otras
            preferencias.
          </span>
        </div>
      )}

      {/* Totales vs objetivo */}
      <div className="grid grid-cols-2 sm:grid-cols-4 gap-2 sm:gap-3 mb-5">
        <TotalBox
          label="Calorías"
          real={plan.kcal_total}
          objetivo={kcalObjetivo}
          unidad="kcal"
          tone="text-ember"
        />
        <TotalBox
          label="Proteína"
          real={plan.macros_totales.proteinas}
          objetivo={macrosObjetivo.proteinas}
          unidad="g"
          tone="text-moss"
        />
        <TotalBox
          label="Grasas"
          real={plan.macros_totales.grasas}
          objetivo={macrosObjetivo.grasas}
          unidad="g"
          tone="text-ochre"
        />
        <TotalBox
          label="Carbos"
          real={plan.macros_totales.carbohidratos}
          objetivo={macrosObjetivo.carbohidratos}
          unidad="g"
          tone="text-sky"
        />
      </div>

      <div className="space-y-4">
        {plan.comidas.map((comida, i) => (
          <ComidaCard key={comida.nombre} comida={comida} index={i} />
        ))}
      </div>

      <p className="mt-4 font-mono text-[10px] uppercase tracking-wider text-ink/40">
        valores nutricionales declarados por los fabricantes · open food facts
      </p>
    </Card>
  );
}

function TotalBox({
  label,
  real,
  objetivo,
  unidad,
  tone,
}: {
  label: string;
  real: number;
  objetivo: number;
  unidad: string;
  tone: string;
}) {
  const delta = objetivo > 0 ? ((real - objetivo) / objetivo) * 100 : 0;
  return (
    <div className="border-2 border-ink bg-sand/40 px-3 py-2.5">
      <div className="font-mono text-[9px] uppercase tracking-[0.2em] text-ink/50">{label}</div>
      <div className={cn("font-display text-lg sm:text-xl font-semibold tabular-nums", tone)}>
        {Math.round(real)}
        <span className="font-mono text-[10px] text-ink/50 ml-1">{unidad}</span>
      </div>
      <div className="font-mono text-[10px] text-ink/50 tabular-nums">
        obj. {Math.round(objetivo)} ({delta >= 0 ? "+" : ""}
        {delta.toFixed(1)}%)
      </div>
    </div>
  );
}

function ComidaCard({ comida, index }: { comida: ComidaPlan; index: number }) {
  return (
    <motion.div
      initial={{ opacity: 0, y: 10 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ delay: index * 0.06, duration: 0.3 }}
      className="border-2 border-ink bg-surface"
    >
      <div className="bg-ink text-sand px-3.5 sm:px-4 py-2.5 flex items-baseline justify-between gap-2 flex-wrap">
        <span className="font-display font-semibold">{comida.nombre}</span>
        <span className="font-mono text-[11px] text-sand/70 tabular-nums">
          <span className="text-ember font-bold">{Math.round(comida.kcal)} kcal</span> · P{" "}
          {Math.round(comida.macros.proteinas)} · G {Math.round(comida.macros.grasas)} · C{" "}
          {Math.round(comida.macros.carbohidratos)}
        </span>
      </div>
      <ul className="divide-y divide-dotted divide-ink/25">
        {comida.productos.map((p) => (
          <li
            key={`${p.nombre}-${p.cantidad_g}`}
            className="px-3.5 sm:px-4 py-2.5 flex items-baseline justify-between gap-3 text-sm"
          >
            <span className="min-w-0">
              <span className="font-medium">{p.nombre}</span>
              {p.marca && <span className="text-ink/50 text-xs"> · {p.marca}</span>}
            </span>
            <span className="font-mono text-xs text-ink/70 tabular-nums flex-shrink-0">
              {Math.round(p.cantidad_g)} g · {Math.round(p.kcal)} kcal
            </span>
          </li>
        ))}
      </ul>
    </motion.div>
  );
}
