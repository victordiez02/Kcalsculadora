import { ChefHat, Sparkles } from "lucide-react";

import { Button } from "@/components/ui/button";
import { Card } from "@/components/ui/card";

/** CTA destacado, justo bajo el hero de calorías, para lanzar el wizard de dieta. */
export function GenerarPlanCTA({ onClick }: { onClick: () => void }) {
  return (
    <Card className="relative overflow-hidden bg-moss/15 p-5 sm:p-6">
      {/* Textura diagonal sutil, en la línea del fondo punteado de la app */}
      <div
        aria-hidden
        className="pointer-events-none absolute inset-0 opacity-[0.06]"
        style={{
          backgroundImage:
            "repeating-linear-gradient(-45deg, rgb(var(--moss)) 0 2px, transparent 2px 14px)",
        }}
      />

      <div className="relative flex flex-col sm:flex-row sm:items-center justify-between gap-4 sm:gap-6">
        <div className="flex items-start gap-3.5">
          <span className="flex-shrink-0 w-11 h-11 sm:w-12 sm:h-12 border-2 border-ink bg-moss text-white flex items-center justify-center shadow-notchSm">
            <ChefHat size={22} />
          </span>
          <div>
            <div className="flex items-center gap-1.5 font-mono text-[10px] uppercase tracking-[0.3em] text-ink/60 mb-1">
              <span className="w-1.5 h-1.5 bg-moss inline-block" /> siguiente paso
            </div>
            <div className="font-display text-lg sm:text-xl font-semibold leading-tight">
              ¿Y esto cómo se come?
            </div>
            <p className="text-sm text-ink/65 mt-1 max-w-md">
              Convierte tus kcal y macros en un menú del día con productos reales de tu
              supermercado, listo para descargar en PDF.
            </p>
          </div>
        </div>
        <Button onClick={onClick} type="button" className="w-full sm:w-auto flex-shrink-0">
          <Sparkles size={17} /> Generar plan de comidas
        </Button>
      </div>
    </Card>
  );
}
