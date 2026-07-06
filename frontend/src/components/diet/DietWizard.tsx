import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { AnimatePresence, motion } from "framer-motion";
import {
  ArrowLeft,
  ArrowRight,
  Ban,
  Check,
  ChefHat,
  Heart,
  Plus,
  RefreshCcw,
  ShieldAlert,
  ShoppingBasket,
  Sparkles,
  UtensilsCrossed,
} from "lucide-react";

import { despertarDietApi, generarDieta } from "@/api/dietApi";
import {
  EVITAR_SUGERENCIAS,
  FAVORITOS_SUGERENCIAS,
  INTOLERANCIAS,
  NUM_COMIDAS_OPCIONES,
  SUPERMERCADOS,
  VARIEDADES,
} from "@/constants";
import type { DietOutput, Intolerancia, Macros, Objetivo, Supermercado, Variedad } from "@/types";
import { Button } from "@/components/ui/button";
import { Dialog, DialogContent, DialogDescription, DialogTitle } from "@/components/ui/dialog";
import { Segmented } from "@/components/ui/segmented";
import { cn } from "@/lib/utils";

interface Prefs {
  supermercado: Supermercado | null;
  numComidas: "3" | "4" | "5";
  intolerancias: Intolerancia[];
  evitar: string[];
  favoritos: string[];
  variedad: Variedad;
}

const PREFS_INICIALES: Prefs = {
  supermercado: null,
  numComidas: "4",
  intolerancias: [],
  evitar: [],
  favoritos: [],
  variedad: "sin_repetir",
};

const MENSAJES_CARGA = [
  "Repartiendo tus kcal y macros entre las comidas…",
  "Buscando productos reales en el supermercado…",
  "Eligiendo cantidades para cuadrar tus macros…",
  "Validando que el plan cumpla tu objetivo…",
  "Dando los últimos retoques…",
];

export function DietWizard({
  open,
  onOpenChange,
  kcalObjetivo,
  macrosObjetivo,
  objetivo,
  onPlanGenerado,
}: {
  open: boolean;
  onOpenChange: (v: boolean) => void;
  kcalObjetivo: number;
  macrosObjetivo: Macros;
  objetivo: Objetivo;
  onPlanGenerado: (plan: DietOutput, supermercado: Supermercado) => void;
}) {
  const [paso, setPaso] = useState(0);
  const [prefs, setPrefs] = useState<Prefs>(PREFS_INICIALES);
  const [generando, setGenerando] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const direccion = useRef(1);

  // Despierta el backend (cold start de Cloud Run) en cuanto se abre.
  useEffect(() => {
    if (open) {
      despertarDietApi();
      setError(null);
    }
  }, [open]);

  function set<K extends keyof Prefs>(k: K, v: Prefs[K]) {
    setPrefs((p) => ({ ...p, [k]: v }));
  }

  function toggleLista<T extends string>(lista: T[], valor: T): T[] {
    return lista.includes(valor) ? lista.filter((x) => x !== valor) : [...lista, valor];
  }

  const pasos = useMemo(
    () => [
      {
        icono: ShoppingBasket,
        titulo: "¿Dónde haces la compra?",
        descripcion: "El plan usará solo productos reales de este supermercado.",
        valido: prefs.supermercado !== null,
        contenido: (
          <div className="grid grid-cols-2 gap-2.5">
            {SUPERMERCADOS.map((s) => {
              const activo = prefs.supermercado === s.id;
              return (
                <button
                  key={s.id}
                  type="button"
                  aria-pressed={activo}
                  onClick={() => set("supermercado", s.id)}
                  className={cn(
                    "relative flex flex-col items-center gap-2.5 border-2 border-ink py-5 px-3 transition",
                    activo ? "bg-sand shadow-notchSm" : "bg-surface hover:bg-sand/60",
                  )}
                >
                  {activo && (
                    <span className="absolute top-1.5 right-1.5 w-5 h-5 bg-ember text-white flex items-center justify-center border border-ink">
                      <Check size={12} strokeWidth={3} />
                    </span>
                  )}
                  <span
                    className="w-11 h-11 flex items-center justify-center border-2 border-ink font-display text-2xl font-bold text-white"
                    style={{ backgroundColor: s.color }}
                  >
                    {s.label[0]}
                  </span>
                  <span className="font-mono text-xs uppercase tracking-wider">{s.label}</span>
                </button>
              );
            })}
          </div>
        ),
      },
      {
        icono: UtensilsCrossed,
        titulo: "¿Cuántas comidas haces al día?",
        descripcion: "Repartiremos tus calorías y macros entre ellas.",
        valido: true,
        contenido: (
          <div className="space-y-3">
            <Segmented
              value={prefs.numComidas}
              onChange={(v) => set("numComidas", v)}
              activeTone="ember"
              items={NUM_COMIDAS_OPCIONES.map((o) => ({ id: o.id, label: o.label }))}
            />
            <p className="font-mono text-xs text-ink/60 text-center">
              {NUM_COMIDAS_OPCIONES.find((o) => o.id === prefs.numComidas)?.hint}
            </p>
          </div>
        ),
      },
      {
        icono: ShieldAlert,
        titulo: "¿Alguna intolerancia o alergia?",
        descripcion:
          "Se excluyen de forma estricta usando los alérgenos declarados de cada producto.",
        valido: true,
        contenido: (
          <div className="flex flex-wrap gap-2">
            {INTOLERANCIAS.map((i) => (
              <Chip
                key={i.id}
                activo={prefs.intolerancias.includes(i.id)}
                tono="clay"
                onClick={() => set("intolerancias", toggleLista(prefs.intolerancias, i.id))}
              >
                {i.label}
              </Chip>
            ))}
          </div>
        ),
      },
      {
        icono: Ban,
        titulo: "¿Qué alimentos prefieres evitar?",
        descripcion: "No aparecerán en tu plan. Puedes añadir los tuyos.",
        valido: true,
        contenido: (
          <ChipsConLibre
            sugerencias={EVITAR_SUGERENCIAS}
            seleccion={prefs.evitar}
            tono="clay"
            placeholder="otro alimento a evitar…"
            onChange={(v) => set("evitar", v)}
          />
        ),
      },
      {
        icono: Heart,
        titulo: "¿Cuáles son tus favoritos?",
        descripcion: "Les daremos prioridad al montar las comidas.",
        valido: true,
        contenido: (
          <ChipsConLibre
            sugerencias={FAVORITOS_SUGERENCIAS}
            seleccion={prefs.favoritos}
            tono="moss"
            placeholder="otro alimento favorito…"
            onChange={(v) => set("favoritos", v)}
          />
        ),
      },
      {
        icono: RefreshCcw,
        titulo: "¿Cuánta variedad quieres?",
        descripcion: "Repetir productos hace más fácil clavar los números.",
        valido: true,
        contenido: (
          <div className="grid gap-2.5">
            {VARIEDADES.map((v) => {
              const activo = prefs.variedad === v.id;
              return (
                <button
                  key={v.id}
                  type="button"
                  aria-pressed={activo}
                  onClick={() => set("variedad", v.id)}
                  className={cn(
                    "flex items-center justify-between border-2 border-ink px-4 py-3.5 transition text-left",
                    activo ? "bg-ink text-sand shadow-notchSm" : "bg-surface hover:bg-sand/60",
                  )}
                >
                  <span>
                    <span className="font-mono text-sm uppercase tracking-wider block">
                      {v.label}
                    </span>
                    <span className={cn("text-xs", activo ? "text-sand/70" : "text-ink/60")}>
                      {v.hint}
                    </span>
                  </span>
                  {activo && <Check size={18} className="text-ember flex-shrink-0" />}
                </button>
              );
            })}
          </div>
        ),
      },
      {
        icono: ChefHat,
        titulo: "Todo listo",
        descripcion: "Revisa tus preferencias antes de generar el plan.",
        valido: true,
        contenido: (
          <ResumenPrefs prefs={prefs} kcalObjetivo={kcalObjetivo} macrosObjetivo={macrosObjetivo} />
        ),
      },
    ],
    [prefs, kcalObjetivo, macrosObjetivo],
  );

  const esUltimo = paso === pasos.length - 1;
  const pasoActual = pasos[paso];

  function avanzar() {
    direccion.current = 1;
    setPaso((p) => Math.min(p + 1, pasos.length - 1));
  }

  function retroceder() {
    direccion.current = -1;
    setError(null);
    setPaso((p) => Math.max(p - 1, 0));
  }

  const generar = useCallback(async () => {
    if (!prefs.supermercado) return;
    setGenerando(true);
    setError(null);
    try {
      const plan = await generarDieta({
        kcal_objetivo: kcalObjetivo,
        macros_objetivo: macrosObjetivo,
        objetivo,
        supermercado: prefs.supermercado,
        num_comidas: Number(prefs.numComidas),
        intolerancias: prefs.intolerancias,
        evitar: prefs.evitar,
        favoritos: prefs.favoritos,
        variedad: prefs.variedad,
      });
      onPlanGenerado(plan, prefs.supermercado);
    } catch (e) {
      setError(e instanceof Error ? e.message : "Error desconocido");
    } finally {
      setGenerando(false);
    }
  }, [prefs, kcalObjetivo, macrosObjetivo, objetivo, onPlanGenerado]);

  return (
    <Dialog open={open} onOpenChange={(v) => !generando && onOpenChange(v)}>
      <DialogContent>
        {generando ? (
          <PantallaCarga />
        ) : (
          <div className="p-5 sm:p-6">
            {/* Progreso */}
            <div className="flex items-center gap-2 mb-5 pr-10">
              <div className="flex-1 h-2 border-2 border-ink bg-sand/50">
                <motion.div
                  className="h-full bg-ember"
                  animate={{ width: `${((paso + 1) / pasos.length) * 100}%` }}
                  transition={{ type: "spring", stiffness: 260, damping: 30 }}
                />
              </div>
              <span className="font-mono text-[10px] uppercase tracking-wider text-ink/50">
                {paso + 1}/{pasos.length}
              </span>
            </div>

            <AnimatePresence mode="wait" custom={direccion.current}>
              <motion.div
                key={paso}
                initial={{ opacity: 0, x: 28 * direccion.current }}
                animate={{ opacity: 1, x: 0 }}
                exit={{ opacity: 0, x: -28 * direccion.current }}
                transition={{ duration: 0.22, ease: [0.2, 0.7, 0.3, 1] }}
              >
                <div className="flex items-start gap-3 mb-1.5">
                  <span className="mt-0.5 flex-shrink-0 w-9 h-9 border-2 border-ink bg-ember text-white flex items-center justify-center shadow-notchSm">
                    <pasoActual.icono size={18} />
                  </span>
                  <div>
                    <DialogTitle>{pasoActual.titulo}</DialogTitle>
                    <DialogDescription className="mt-1">{pasoActual.descripcion}</DialogDescription>
                  </div>
                </div>
                <div className="mt-5 min-h-[220px]">{pasoActual.contenido}</div>
              </motion.div>
            </AnimatePresence>

            {error && (
              <div className="mt-4 border-2 border-clay bg-clay/10 px-3 py-2.5 text-sm text-clay font-medium">
                {error}
              </div>
            )}

            <div className="mt-6 flex gap-2.5">
              <Button
                variant="ghost"
                size="sm"
                onClick={retroceder}
                disabled={paso === 0}
                type="button"
              >
                <ArrowLeft size={15} /> Atrás
              </Button>
              {esUltimo ? (
                <Button className="flex-1" size="sm" onClick={generar} type="button">
                  <Sparkles size={16} /> Generar plan de comidas
                </Button>
              ) : (
                <Button
                  className="flex-1"
                  size="sm"
                  onClick={avanzar}
                  disabled={!pasoActual.valido}
                  type="button"
                >
                  Siguiente <ArrowRight size={15} />
                </Button>
              )}
            </div>
          </div>
        )}
      </DialogContent>
    </Dialog>
  );
}

function PantallaCarga() {
  const [idx, setIdx] = useState(0);
  useEffect(() => {
    const t = setInterval(() => setIdx((i) => (i + 1) % MENSAJES_CARGA.length), 4000);
    return () => clearInterval(t);
  }, []);
  return (
    <div className="p-8 sm:p-10 text-center">
      <DialogTitle className="sr-only">Generando plan de comidas</DialogTitle>
      <motion.div
        className="mx-auto w-16 h-16 border-2 border-ink bg-ember text-white flex items-center justify-center shadow-notch mb-6"
        animate={{ rotate: [0, -6, 6, 0] }}
        transition={{ repeat: Infinity, duration: 1.6, ease: "easeInOut" }}
      >
        <ChefHat size={30} />
      </motion.div>
      <div className="font-display text-xl font-semibold mb-2">Cocinando tu plan…</div>
      <AnimatePresence mode="wait">
        <motion.p
          key={idx}
          initial={{ opacity: 0, y: 6 }}
          animate={{ opacity: 1, y: 0 }}
          exit={{ opacity: 0, y: -6 }}
          className="font-mono text-xs text-ink/60 min-h-[2rem]"
        >
          {MENSAJES_CARGA[idx]}
        </motion.p>
      </AnimatePresence>
      <div className="mt-4 font-mono text-[10px] uppercase tracking-wider text-ink/40">
        suele tardar 20–60 segundos
      </div>
    </div>
  );
}

function Chip({
  activo,
  tono,
  onClick,
  children,
}: {
  activo: boolean;
  tono: "clay" | "moss";
  onClick: () => void;
  children: React.ReactNode;
}) {
  return (
    <button
      type="button"
      aria-pressed={activo}
      onClick={onClick}
      className={cn(
        "border-2 border-ink px-3 py-2 font-mono text-xs uppercase tracking-wider transition select-none",
        "active:translate-x-[1px] active:translate-y-[1px]",
        activo
          ? tono === "clay"
            ? "bg-clay text-white shadow-notchSm"
            : "bg-moss text-white shadow-notchSm"
          : "bg-surface text-ink hover:bg-sand/60",
      )}
    >
      {children}
    </button>
  );
}

function ChipsConLibre({
  sugerencias,
  seleccion,
  tono,
  placeholder,
  onChange,
}: {
  sugerencias: readonly string[];
  seleccion: string[];
  tono: "clay" | "moss";
  placeholder: string;
  onChange: (v: string[]) => void;
}) {
  const [libre, setLibre] = useState("");
  const personalizados = seleccion.filter((s) => !sugerencias.includes(s));

  function toggle(valor: string) {
    onChange(
      seleccion.includes(valor) ? seleccion.filter((x) => x !== valor) : [...seleccion, valor],
    );
  }

  function añadirLibre() {
    const v = libre.trim().toLowerCase();
    if (v && !seleccion.includes(v)) onChange([...seleccion, v]);
    setLibre("");
  }

  return (
    <div className="space-y-3">
      <div className="flex flex-wrap gap-2">
        {sugerencias.map((s) => (
          <Chip key={s} activo={seleccion.includes(s)} tono={tono} onClick={() => toggle(s)}>
            {s}
          </Chip>
        ))}
        {personalizados.map((s) => (
          <Chip key={s} activo tono={tono} onClick={() => toggle(s)}>
            {s} ×
          </Chip>
        ))}
      </div>
      <div className="flex items-stretch gap-2">
        <input
          value={libre}
          onChange={(e) => setLibre(e.target.value)}
          onKeyDown={(e) => {
            if (e.key === "Enter") {
              e.preventDefault();
              añadirLibre();
            }
          }}
          placeholder={placeholder}
          className="flex-1 min-w-0 border-2 border-ink bg-sand/50 focus:bg-surface transition px-3 py-2 font-mono text-sm focus:outline-none"
        />
        <Button variant="ghost" size="sm" type="button" onClick={añadirLibre} aria-label="Añadir">
          <Plus size={15} />
        </Button>
      </div>
    </div>
  );
}

function ResumenPrefs({
  prefs,
  kcalObjetivo,
  macrosObjetivo,
}: {
  prefs: Prefs;
  kcalObjetivo: number;
  macrosObjetivo: Macros;
}) {
  const superLabel = SUPERMERCADOS.find((s) => s.id === prefs.supermercado)?.label ?? "—";
  const filas: [string, string][] = [
    ["Supermercado", superLabel],
    ["Comidas al día", prefs.numComidas],
    [
      "Intolerancias",
      prefs.intolerancias.length
        ? prefs.intolerancias
            .map((i) => INTOLERANCIAS.find((x) => x.id === i)?.label ?? i)
            .join(", ")
        : "ninguna",
    ],
    ["Evitar", prefs.evitar.length ? prefs.evitar.join(", ") : "nada en particular"],
    ["Favoritos", prefs.favoritos.length ? prefs.favoritos.join(", ") : "sin preferencias"],
    ["Variedad", VARIEDADES.find((v) => v.id === prefs.variedad)?.label ?? ""],
  ];
  return (
    <div className="space-y-3">
      <div className="border-2 border-ink bg-ink text-sand px-4 py-3 flex items-baseline justify-between">
        <span className="font-mono text-[10px] uppercase tracking-[0.25em] text-sand/60">
          objetivo diario
        </span>
        <span className="font-display font-semibold">
          {Math.round(kcalObjetivo)} kcal · P {Math.round(macrosObjetivo.proteinas)} · G{" "}
          {Math.round(macrosObjetivo.grasas)} · C {Math.round(macrosObjetivo.carbohidratos)}
        </span>
      </div>
      <div className="border-2 border-ink divide-y-2 divide-ink/15 bg-surface">
        {filas.map(([label, valor]) => (
          <div key={label} className="flex justify-between gap-4 px-4 py-2.5 text-sm">
            <span className="font-mono text-[11px] uppercase tracking-wider text-ink/50 pt-0.5">
              {label}
            </span>
            <span className="text-right font-medium">{valor}</span>
          </div>
        ))}
      </div>
    </div>
  );
}
