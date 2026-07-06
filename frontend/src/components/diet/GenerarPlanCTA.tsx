import { ChefHat, Sparkles } from "lucide-react";

import { Button } from "@/components/ui/button";
import { Card } from "@/components/ui/card";

/** CTA que aparece bajo los resultados del cálculo para lanzar el wizard. */
export function GenerarPlanCTA({ onClick }: { onClick: () => void }) {
  return (
    <Card className="p-5 sm:p-6 flex flex-col sm:flex-row items-start sm:items-center gap-4 justify-between">
      <div className="flex items-start gap-3">
        <span className="flex-shrink-0 w-10 h-10 border-2 border-ink bg-moss text-white flex items-center justify-center shadow-notchSm">
          <ChefHat size={20} />
        </span>
        <div>
          <div className="font-display text-lg sm:text-xl font-semibold leading-tight">
            ¿Y esto cómo se come?
          </div>
          <p className="text-sm text-ink/60 mt-0.5 max-w-md">
            Convierte tus kcal y macros en un menú del día con productos reales de tu supermercado,
            listo para descargar en PDF.
          </p>
        </div>
      </div>
      <Button onClick={onClick} type="button" className="w-full sm:w-auto flex-shrink-0">
        <Sparkles size={17} /> Generar plan de comidas
      </Button>
    </Card>
  );
}
