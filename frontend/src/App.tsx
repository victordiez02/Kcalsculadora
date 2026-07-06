import { useCallback, useEffect, useMemo, useState } from "react";
import { AnimatePresence, motion } from "framer-motion";
import {
  Activity,
  Calculator,
  Dumbbell,
  Flame,
  HelpCircle,
  Mail,
  Mars,
  Moon,
  RotateCcw,
  Scale,
  Sparkles,
  Sun,
  Venus,
} from "lucide-react";

import { calcular } from "@/api";
import {
  ACTIVIDADES_HELMS,
  ACTIVIDADES_MIFFLIN,
  AGRESIVIDADES,
  ENFOQUES_RECOMP,
  LIMITES,
  NIVELES,
  OBJETIVOS,
  SEXOS,
} from "@/constants";
import type {
  Agresividad,
  CalculoInput,
  CalculoOutput,
  EnfoqueRecomp,
  Nivel,
  Objetivo,
  Sexo,
} from "@/types";
import { AvisoBanner } from "@/components/AvisoBanner";
import { Explicacion } from "@/components/Explicacion";
import { ImcBar } from "@/components/ImcBar";
import { MacrosCard } from "@/components/MacrosCard";
import { ResumenPlan } from "@/components/ResumenPlan";
import { Alert, AlertDescription } from "@/components/ui/alert";
import { Button } from "@/components/ui/button";
import { Card } from "@/components/ui/card";
import { Checkbox } from "@/components/ui/checkbox";
import { Label } from "@/components/ui/label";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { Segmented } from "@/components/ui/segmented";
import { Tooltip, TooltipContent, TooltipTrigger } from "@/components/ui/tooltip";
import { useTheme } from "@/lib/use-theme";

interface Form {
  peso: number;
  altura: number;
  edad: number;
  sexo: Sexo;
  conoceGrasa: boolean;
  grasa: number;
  nivel: Nivel;
  actividad: number;
  objetivo: Objetivo;
  agresividad: Agresividad;
  enfoqueRecomp: EnfoqueRecomp;
}

interface BaseInputs {
  peso: number;
  altura: number;
  edad: number;
  sexo: Sexo;
  actividad: number;
  objetivo: Objetivo;
}

const DEFAULTS: Form = {
  peso: 70,
  altura: 170,
  edad: 25,
  sexo: "M",
  conoceGrasa: false,
  grasa: 15,
  nivel: "principiante",
  actividad: 1.55,
  objetivo: "mant",
  agresividad: "moderada",
  enfoqueRecomp: "equilibrio",
};

const STAT_NUM = "font-display font-semibold text-ink leading-none tabular-nums";
const FORM_KEY = "kcals:form:v2";

function loadJSON<T>(key: string, fallback: T): T {
  try {
    const raw = window.localStorage.getItem(key);
    return raw ? (JSON.parse(raw) as T) : fallback;
  } catch {
    return fallback;
  }
}

function saveJSON(key: string, value: unknown) {
  try {
    window.localStorage.setItem(key, JSON.stringify(value));
  } catch {
    // storage no disponible (modo privado)
  }
}

export default function App() {
  const [form, setForm] = useState<Form>(() => ({
    ...DEFAULTS,
    ...loadJSON<Partial<Form>>(FORM_KEY, {}),
  }));
  const [resultado, setResultado] = useState<CalculoOutput | null>(null);
  const [baseInputs, setBaseInputs] = useState<BaseInputs>(() => ({
    peso: form.peso,
    altura: form.altura,
    edad: form.edad,
    sexo: form.sexo,
    actividad: form.actividad,
    objetivo: form.objetivo,
  }));
  const [cargando, setCargando] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const { theme, toggle: toggleTheme } = useTheme();

  const actividades = useMemo(
    () => (form.nivel === "avanzado" ? ACTIVIDADES_HELMS : ACTIVIDADES_MIFFLIN),
    [form.nivel],
  );

  useEffect(() => {
    if (!actividades.some((a) => a.value === form.actividad)) {
      setForm((f) => ({ ...f, actividad: actividades[0].value }));
    }
  }, [actividades, form.actividad]);

  useEffect(() => saveJSON(FORM_KEY, form), [form]);

  useEffect(() => {
    const url = import.meta.env.VITE_API_URL;
    if (url) {
      fetch(`${url}/health`);
    }
  }, []);

  function set<K extends keyof Form>(k: K, v: Form[K]) {
    setForm((f) => ({ ...f, [k]: v }));
  }

  const onCalcular = useCallback(async () => {
    setError(null);
    setCargando(true);
    try {
      const payload: CalculoInput = {
        peso: form.peso,
        altura: form.altura,
        edad: form.edad,
        sexo: form.sexo,
        grasa: form.conoceGrasa ? form.grasa : null,
        nivel: form.nivel,
        actividad: form.actividad,
        objetivo: form.objetivo,
        agresividad: form.agresividad,
        enfoque_recomp: form.enfoqueRecomp,
      };
      const out = await calcular(payload);
      setResultado(out);
      setBaseInputs({
        peso: form.peso,
        altura: form.altura,
        edad: form.edad,
        sexo: form.sexo,
        actividad: form.actividad,
        objetivo: form.objetivo,
      });
      requestAnimationFrame(() => {
        document
          .getElementById("resultados")
          ?.scrollIntoView({ behavior: "smooth", block: "start" });
      });
    } catch (e) {
      setError(e instanceof Error ? e.message : "Error desconocido");
      setResultado(null);
    } finally {
      setCargando(false);
    }
  }, [form]);

  function reiniciar() {
    setForm(DEFAULTS);
    setResultado(null);
    setError(null);
    try {
      window.localStorage.removeItem(FORM_KEY);
    } catch {
      // ignora
    }
  }

  const imcObjetivo = useMemo(() => {
    if (!resultado || !baseInputs.altura) return null;
    const h = baseInputs.altura / 100;
    return resultado.peso_objetivo / (h * h);
  }, [resultado, baseInputs.altura]);

  return (
    <div className="min-h-screen">
      <Header theme={theme} onToggleTheme={toggleTheme} />

      <main className="max-w-6xl mx-auto px-4 sm:px-5 pb-24 grid gap-6 sm:gap-8 lg:grid-cols-[420px_1fr] lg:items-start">
        <Card className="p-4 sm:p-6">
          <div className="flex items-baseline justify-between mb-4 sm:mb-5">
            <h2 className="font-display text-xl sm:text-2xl font-semibold flex items-center gap-2">
              <Calculator size={20} className="text-ember" /> Tus datos
            </h2>
          </div>

          <div className="grid grid-cols-2 gap-3 sm:gap-4 mb-4">
            <NumField
              id="peso"
              label="Peso"
              value={form.peso}
              onChange={(v) => set("peso", v)}
              limits={LIMITES.peso}
              suffix="kg"
            />
            <NumField
              id="altura"
              label="Altura"
              value={form.altura}
              onChange={(v) => set("altura", v)}
              limits={LIMITES.altura}
              suffix="cm"
            />
            <NumField
              id="edad"
              label="Edad"
              value={form.edad}
              onChange={(v) => set("edad", v)}
              limits={LIMITES.edad}
              suffix="años"
            />
            <div>
              <LabelWithHelp
                htmlFor="sexo"
                tooltip="Mifflin-St Jeor usa un offset distinto para hombres y mujeres."
              >
                Sexo
              </LabelWithHelp>
              <Segmented
                value={form.sexo}
                onChange={(v) => set("sexo", v)}
                items={SEXOS.map((s) => ({ id: s.id, label: s.label }))}
                renderItem={(it) =>
                  it.id === "M" ? (
                    <Mars size={18} strokeWidth={2} />
                  ) : (
                    <Venus size={18} strokeWidth={2} />
                  )
                }
              />
            </div>
          </div>

          <div className="mb-4">
            <label className="flex items-center gap-3 cursor-pointer select-none">
              <Checkbox
                checked={form.conoceGrasa}
                onCheckedChange={(v) => set("conoceGrasa", v === true)}
              />
              <span className="text-sm flex items-center gap-1.5">
                Conozco mi % de grasa corporal
                <InfoIcon tooltip="Si no la indicas, se estima por la fórmula de Deurenberg (IMC + edad + sexo). La estimación es orientativa: si tienes una medición (DEXA, BIA fiable, antropometría), indícala." />
              </span>
            </label>
            <AnimatePresence initial={false} mode="wait">
              {form.conoceGrasa ? (
                <motion.div
                  key="grasa-input"
                  initial={{ opacity: 0, height: 0 }}
                  animate={{ opacity: 1, height: "auto" }}
                  exit={{ opacity: 0, height: 0 }}
                  className="overflow-hidden"
                >
                  <div className="mt-3">
                    <NumField
                      id="grasa"
                      label="Grasa corporal"
                      value={form.grasa}
                      onChange={(v) => set("grasa", v)}
                      limits={LIMITES.grasa}
                      suffix="%"
                    />
                  </div>
                </motion.div>
              ) : null}
            </AnimatePresence>
          </div>

          <div className="mb-4">
            <LabelWithHelp tooltip="Determina qué tabla de factor de actividad se aplica y los g/kg de proteína por defecto.">
              Nivel de entrenamiento
            </LabelWithHelp>
            <Segmented
              value={form.nivel}
              onChange={(v) => set("nivel", v)}
              columns={3}
              items={NIVELES.map((n) => ({
                id: n.id,
                label: n.label,
                hint: n.hint,
                title: n.hint,
              }))}
              renderItem={(it) => (
                <span className="flex flex-col items-center leading-tight">
                  <span className="font-mono text-[11px] sm:text-xs uppercase tracking-wider">
                    {it.label}
                  </span>
                  <span className="text-[10px] opacity-70 hidden sm:block normal-case tracking-normal">
                    {it.hint}
                  </span>
                </span>
              )}
            />
          </div>

          <div className="mb-4">
            <LabelWithHelp
              htmlFor="actividad"
              tooltip={
                form.nivel === "avanzado"
                  ? "Tabla de Eric Helms: pensada para personas que ya entrenan fuerza de forma estructurada."
                  : "Tabla clásica Mifflin-St Jeor: factor multiplicador sobre la TMB."
              }
            >
              <span className="inline-flex items-center gap-1.5">
                <Activity size={12} /> Nivel de actividad (
                {form.nivel === "avanzado" ? "Eric Helms" : "Mifflin-St Jeor"})
              </span>
            </LabelWithHelp>
            <Select
              value={String(form.actividad)}
              onValueChange={(v) => set("actividad", parseFloat(v))}
            >
              <SelectTrigger id="actividad">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {actividades.map((a) => (
                  <SelectItem key={a.value} value={String(a.value)}>
                    {a.label} — {a.hint} (×{a.value})
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          <div className="mb-4">
            <LabelWithHelp tooltip="Definir = perder grasa. Volumen = ganar masa. Recomposición = ganar músculo y perder grasa a la vez (más lento).">
              Objetivo
            </LabelWithHelp>
            <Segmented
              value={form.objetivo}
              onChange={(v) => set("objetivo", v)}
              activeTone="ember"
              items={OBJETIVOS.map((o) => ({ id: o.id, label: o.label }))}
            />
          </div>

          <AnimatePresence initial={false}>
            {form.objetivo !== "mant" && (
              <motion.div
                key="agresividad"
                initial={{ opacity: 0, height: 0 }}
                animate={{ opacity: 1, height: "auto" }}
                exit={{ opacity: 0, height: 0 }}
                className="overflow-hidden"
              >
                <div className="mb-5">
                  <LabelWithHelp tooltip="A mayor ritmo, mayor déficit (definición) o superávit (volumen). También más riesgo de perder músculo o ganar grasa.">
                    Ritmo del cambio
                  </LabelWithHelp>
                  <AgresividadSlider
                    value={form.agresividad}
                    onChange={(v) => set("agresividad", v)}
                  />
                </div>
              </motion.div>
            )}
            {form.objetivo === "mant" && (
              <motion.div
                key="enfoque-recomp"
                initial={{ opacity: 0, height: 0 }}
                animate={{ opacity: 1, height: "auto" }}
                exit={{ opacity: 0, height: 0 }}
                className="overflow-hidden"
              >
                <div className="mb-5">
                  <LabelWithHelp tooltip="En recomposición eliges hacia dónde inclinar la balanza: ganar algo más de músculo, mantener el balance, o sacrificar un poco de músculo para definir más.">
                    Enfoque de la recomposición
                  </LabelWithHelp>
                  <EnfoqueRecompSelector
                    value={form.enfoqueRecomp}
                    onChange={(v) => set("enfoqueRecomp", v)}
                  />
                </div>
              </motion.div>
            )}
          </AnimatePresence>

          <div className="flex gap-3">
            <Button className="flex-1" onClick={onCalcular} disabled={cargando}>
              {cargando ? (
                <>
                  <Sparkles size={18} className="animate-pulse" /> Calculando…
                </>
              ) : (
                <>
                  <Calculator size={18} /> Calcular
                </>
              )}
            </Button>
            <Tooltip>
              <TooltipTrigger asChild>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={reiniciar}
                  type="button"
                  aria-label="Reiniciar"
                >
                  <RotateCcw size={16} />
                  <span className="hidden sm:inline">Reiniciar</span>
                </Button>
              </TooltipTrigger>
              <TooltipContent>
                Restaura valores por defecto y borra resultados guardados.
              </TooltipContent>
            </Tooltip>
          </div>

          <AnimatePresence>
            {error && (
              <motion.div
                initial={{ opacity: 0, y: -4 }}
                animate={{ opacity: 1, y: 0 }}
                exit={{ opacity: 0 }}
                className="mt-4"
              >
                <Alert variant="danger">
                  <AlertDescription>{error}</AlertDescription>
                </Alert>
              </motion.div>
            )}
          </AnimatePresence>
        </Card>

        <section id="resultados" className="space-y-5 sm:space-y-6 scroll-mt-6">
          {!resultado ? (
            <Placeholder />
          ) : (
            <AnimatePresence mode="wait">
              <motion.div
                key="res"
                initial={{ opacity: 0, y: 12 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ duration: 0.4, ease: [0.2, 0.7, 0.3, 1] }}
                className="space-y-5 sm:space-y-6"
              >
                {resultado.avisos.map((a, i) => (
                  <AvisoBanner key={i} aviso={a} />
                ))}

                <CaloriasHero data={resultado} />

                <div className="grid grid-cols-2 gap-4 sm:gap-6">
                  <StatBox
                    label="TMB"
                    value={`${resultado.tmb}`}
                    unit="kcal/día"
                    hint="Mifflin-St Jeor"
                    tooltip="Tasa metabólica basal: calorías que tu cuerpo gasta en reposo absoluto."
                  />
                  <StatBox
                    label="GET"
                    value={`${resultado.get}`}
                    unit="kcal/día"
                    hint={`actividad ×${resultado.factor_actividad}`}
                    tooltip="Gasto energético total: TMB ajustada por tu factor de actividad diario."
                  />
                  <StatBox
                    label="Masa magra"
                    value={`${resultado.masa_magra}`}
                    unit="kg"
                    hint={`${(100 - (resultado.masa_magra / baseInputs.peso) * 100).toFixed(1)}% grasa actual`}
                    tooltip="Peso total menos la grasa corporal. Es la masa sobre la que se calculan los macros."
                  />
                  <StatBox
                    label="% Grasa"
                    value={`${resultado.grasa_estimada}`}
                    unit="%"
                    hint={
                      resultado.grasa_es_estimada
                        ? "estimada · indícala si la conoces"
                        : "valor introducido"
                    }
                    tooltip={
                      resultado.grasa_es_estimada
                        ? "Estimada por la fórmula de Deurenberg (IMC + edad + sexo). Es orientativa: una medición real (DEXA, BIA fiable) mejora la precisión de macros y peso objetivo."
                        : "% de grasa que has introducido."
                    }
                    badge={resultado.grasa_es_estimada ? "estimada" : undefined}
                  />
                  <StatBox
                    label="Peso objetivo"
                    value={`${resultado.peso_objetivo}`}
                    unit="kg"
                    hint={`meta: ${resultado.grasa_objetivo}% grasa`}
                    tooltip="Peso al que llegarías manteniendo tu masa magra y alcanzando el % de grasa objetivo."
                  />
                  <StatBox
                    label="IMC"
                    value={resultado.imc.toFixed(1)}
                    unit={resultado.imc_categoria.toLowerCase()}
                    hint="peso / altura²"
                    tooltip="Índice de masa corporal. Útil como orientación, no tiene en cuenta composición corporal."
                  />
                </div>

                <MacrosCard macros={resultado.macros} calorias={resultado.calorias_recomendadas} />

                <ImcBar imc={resultado.imc} imcObjetivo={imcObjetivo} />

                <ResumenPlan data={resultado} pesoActual={baseInputs.peso} />

                <Explicacion
                  data={resultado}
                  peso={baseInputs.peso}
                  altura={baseInputs.altura}
                  edad={baseInputs.edad}
                  sexo={baseInputs.sexo}
                  actividad={baseInputs.actividad}
                  objetivo={baseInputs.objetivo}
                />
              </motion.div>
            </AnimatePresence>
          )}
        </section>
      </main>

      <Footer />
    </div>
  );
}

function Header({ theme, onToggleTheme }: { theme: "light" | "dark"; onToggleTheme: () => void }) {
  return (
    <header className="max-w-6xl mx-auto px-4 sm:px-5 pt-6 sm:pt-8 pb-8 sm:pb-10">
      <div className="flex items-center justify-between mb-5 sm:mb-6">
        <div className="flex items-center gap-2 font-mono text-[10px] sm:text-xs uppercase tracking-[0.3em] text-ink/60">
          <span className="w-2 h-2 bg-ember inline-block" /> Calculadora fitness de calorías
        </div>
        <div className="flex items-center gap-3">
          <Tooltip>
            <TooltipTrigger asChild>
              <button
                type="button"
                onClick={onToggleTheme}
                aria-label={theme === "dark" ? "Activar modo claro" : "Activar modo oscuro"}
                className="flex items-center justify-center w-8 h-8 border-2 border-ink bg-surface text-ink hover:bg-sand/60 transition shadow-notchSm active:translate-x-[1px] active:translate-y-[1px] active:shadow-none"
              >
                {theme === "dark" ? <Moon size={14} /> : <Sun size={14} />}
              </button>
            </TooltipTrigger>
            <TooltipContent>
              {theme === "dark" ? "Cambiar a modo claro" : "Cambiar a modo oscuro"}
            </TooltipContent>
          </Tooltip>
          <a
            href="https://github.com/victordiez02/Kcalsculadora"
            target="_blank"
            rel="noreferrer"
            className="flex items-center gap-1.5 font-mono text-[10px] sm:text-xs uppercase tracking-wider text-ink/60 hover:text-ink underline decoration-dotted underline-offset-4"
          >
            <svg
              className="w-3.5 h-3.5 fill-current flex-shrink-0"
              viewBox="0 -0.5 25 25"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path d="m12.301 0h.093c2.242 0 4.34.613 6.137 1.68l-.055-.031c1.871 1.094 3.386 2.609 4.449 4.422l.031.058c1.04 1.769 1.654 3.896 1.654 6.166 0 5.406-3.483 10-8.327 11.658l-.087.026c-.063.02-.135.031-.209.031-.162 0-.312-.054-.433-.144l.002.001c-.128-.115-.208-.281-.208-.466 0-.005 0-.01 0-.014v.001q0-.048.008-1.226t.008-2.154c.007-.075.011-.161.011-.249 0-.792-.323-1.508-.844-2.025.618-.061 1.176-.163 1.718-.305l-.076.017c.573-.16 1.073-.373 1.537-.642l-.031.017c.508-.28.938-.636 1.292-1.058l.006-.007c.372-.476.663-1.036.84-1.645l.009-.035c.209-.683.329-1.468.329-2.281 0-.045 0-.091-.001-.136v.007c0-.022.001-.047.001-.072 0-1.248-.482-2.383-1.269-3.23l.003.003c.168-.44.265-.948.265-1.479 0-.649-.145-1.263-.404-1.814l.011.026c-.115-.022-.246-.035-.381-.035-.334 0-.649.078-.929.216l.012-.005c-.568.21-1.054.448-1.512.726l.038-.022-.609.384c-.922-.264-1.981-.416-3.075-.416s-2.153.152-3.157.436l.081-.02q-.256-.176-.681-.433c-.373-.214-.814-.421-1.272-.595l-.066-.022c-.293-.154-.64-.244-1.009-.244-.124 0-.246.01-.364.03l.013-.002c-.248.524-.393 1.139-.393 1.788 0 .531.097 1.04.275 1.509l-.01-.029c-.785.844-1.266 1.979-1.266 3.227 0 .025 0 .051.001.076v-.004c-.001.039-.001.084-.001.13 0 .809.12 1.591.344 2.327l-.015-.057c.189.643.476 1.202.85 1.693l-.009-.013c.354.435.782.793 1.267 1.062l.022.011c.432.252.933.465 1.46.614l.046.011c.466.125 1.024.227 1.595.284l.046.004c-.431.428-.718 1-.784 1.638l-.001.012c-.207.101-.448.183-.699.236l-.021.004c-.256.051-.549.08-.85.08-.022 0-.044 0-.066 0h.003c-.394-.008-.756-.136-1.055-.348l.006.004c-.371-.259-.671-.595-.881-.986l-.007-.015c-.198-.336-.459-.614-.768-.827l-.009-.006c-.225-.169-.49-.301-.776-.38l-.016-.004-.32-.048c-.023-.002-.05-.003-.077-.003-.14 0-.273.028-.394.077l.007-.003q-.128.072-.08.184c.039.086.087.16.145.225l-.001-.001c.061.072.13.135.205.19l.003.002.112.08c.283.148.516.354.693.603l.004.006c.191.237.359.505.494.792l.01.024.16.368c.135.402.38.738.7.981l.005.004c.3.234.662.402 1.057.478l.016.002c.33.064.714.104 1.106.112h.007c.045.002.097.002.15.002.261 0 .517-.021.767-.062l-.027.004.368-.064q0 .609.008 1.418t.008.873v.014c0 .185-.08.351-.208.466h-.001c-.119.089-.268.143-.431.143-.075 0-.147-.011-.214-.032l.005.001c-4.929-1.689-8.409-6.283-8.409-11.69 0-2.268.612-4.393 1.681-6.219l-.032.058c1.094-1.871 2.609-3.386 4.422-4.449l.058-.031c1.739-1.034 3.835-1.645 6.073-1.645h.098-.005zm-7.64 17.666q.048-.112-.112-.192-.16-.048-.208.032-.048.112.112.192.144.096.208-.032zm.497.545q.112-.08-.032-.256-.16-.144-.256-.048-.112.08.032.256.159.157.256.047zm.48.72q.144-.112 0-.304-.128-.208-.272-.096-.144.08 0 .288t.272.112zm.672.673q.128-.128-.064-.304-.192-.192-.32-.048-.144.128.064.304.192.192.32.044zm.913.4q.048-.176-.208-.256-.24-.064-.304.112t.208.24q.24.097.304-.096zm1.009.08q0-.208-.272-.176-.256 0-.256.176 0 .208.272.176.256.001.256-.175zm.929-.16q-.032-.176-.288-.144-.256.048-.224.24t.288.128.225-.224z" />
            </svg>
            Código
          </a>
        </div>
      </div>
      <h1
        className="font-display font-semibold text-ink leading-[0.95] tracking-tight"
        style={{ fontSize: "clamp(40px, 11vw, 96px)" }}
      >
        <svg
          viewBox="0 0 268 285"
          className="inline-block h-[0.82em] w-auto text-ember align-[-0.08em] mr-[0.01em]"
          aria-hidden="true"
        >
          <path
            d="M 7.506 10.991 C 6.294 12.951, 8.057 13.778, 15.879 14.922 C 25.743 16.364, 30.129 17.601, 31.164 19.232 C 31.607 19.929, 31.976 76.582, 31.985 145.128 C 31.998 251.056, 31.789 269.930, 30.593 270.923 C 29.819 271.566, 24.434 272.880, 18.627 273.844 C 12.820 274.809, 7.784 275.883, 7.434 276.232 C 4.956 278.711, 10.407 279, 59.559 279 C 102.862 279, 111 278.780, 111 277.607 C 111 275.772, 109.750 275.361, 99.848 273.941 C 91.450 272.736, 88.122 271.633, 86.765 269.600 C 86.361 268.995, 86.033 248.025, 86.036 223 L 86.042 177.500 102.086 159.231 C 110.911 149.184, 118.518 141.202, 118.992 141.495 C 120.085 142.171, 122.700 147.010, 152.769 204 C 160.459 218.575, 170.463 237.475, 175 246 C 179.537 254.525, 183.859 263.217, 184.604 265.316 C 185.871 268.887, 185.812 269.279, 183.687 271.403 C 181.912 273.179, 179.795 273.825, 173.980 274.365 C 169.890 274.745, 166.146 275.454, 165.659 275.941 C 162.864 278.736, 167.138 279, 215.132 279 C 260.554 279, 264.123 278.876, 263.822 277.315 C 263.616 276.243, 261.759 275.187, 258.719 274.411 C 252.298 272.773, 247.851 269.175, 243.697 262.258 C 239.224 254.810, 156 99.375, 156 98.469 C 156 96.939, 190.542 58.715, 201.714 47.883 C 220.152 30.006, 237.019 17.541, 246.369 14.883 C 248.091 14.393, 250.175 13.544, 251 12.996 C 254.854 10.436, 248.598 10.023, 209.513 10.259 C 173.279 10.477, 169.497 10.655, 169.210 12.149 C 168.960 13.448, 170.017 13.946, 174.196 14.497 C 192.941 16.971, 200.228 30.503, 191.985 47.532 C 188.900 53.904, 189.672 53, 103.611 150.981 L 86.500 170.461 86.244 95.515 C 86.003 25.165, 86.095 20.461, 87.744 18.819 C 88.870 17.698, 93.267 16.427, 100 15.277 C 107.842 13.938, 110.500 13.107, 110.500 11.992 C 110.500 10.692, 103.913 10.467, 59.315 10.241 C 25.684 10.071, 7.915 10.328, 7.506 10.991 M 215.829 76.486 C 216.284 86.873, 213.461 92.843, 203.046 103.512 C 194.214 112.561, 192.394 116.732, 193.322 125.795 C 194.326 135.591, 201.208 145.435, 210.449 150.291 C 216.346 153.391, 230.786 153.411, 236.551 150.328 C 242.422 147.189, 248.951 140.282, 251.710 134.292 C 256.356 124.204, 253.686 109.498, 245.696 101.168 C 239.829 95.051, 239.422 95.228, 239.851 103.714 C 240.485 116.273, 235.703 122.992, 227.107 121.620 C 222.749 120.925, 219.197 117.565, 218.415 113.399 C 217.955 110.948, 218.514 108.645, 220.621 104.309 C 222.987 99.439, 223.448 97.204, 223.472 90.485 C 223.497 83.403, 223.115 81.687, 220.187 75.735 C 218.365 72.031, 216.565 68.999, 216.187 68.998 C 215.809 68.996, 215.648 72.366, 215.829 76.486"
            stroke="none"
            fill="currentColor"
            fillRule="evenodd"
          />
        </svg>
        cals
        <span className="italic relative">
          culadora
          <svg
            viewBox="0 0 320 16"
            preserveAspectRatio="none"
            className="absolute left-0 -bottom-1 sm:-bottom-2 w-full h-2 sm:h-3 text-ember"
          >
            <path
              d="M2 8 C 80 2, 160 14, 318 6"
              stroke="currentColor"
              strokeWidth="3"
              fill="none"
              strokeLinecap="round"
            />
          </svg>
        </span>
      </h1>
      <p className="mt-5 sm:mt-6 max-w-2xl text-sm sm:text-base text-ink/75 leading-relaxed">
        Una calculadora fitness honesta: introduce tus datos y te devuelve calorías, macros y un
        plan razonable — sin promesas mágicas. Basada en Mifflin-St Jeor, Eric Helms y literatura
        científica al uso.
      </p>
    </header>
  );
}

function Footer() {
  return (
    <footer className="border-t-2 border-ink/15 mt-10">
      <div className="max-w-6xl mx-auto px-4 sm:px-5 py-6 sm:py-8 flex flex-col sm:flex-row gap-3 justify-between items-start sm:items-center font-mono text-xs text-ink/60">
        <div>
          basado en el Shiny original de <strong className="text-ink">Víctor Díez</strong>.
          Reescrito con React + FastAPI.
        </div>
        <div className="flex gap-4 items-center">
          <span>MIT</span>
          <a
            href="mailto:victordiezrecio@gmail.com"
            className="flex items-center gap-1 underline decoration-dotted underline-offset-4 hover:text-ink"
          >
            <Mail size={12} /> contacto
          </a>
        </div>
      </div>
    </footer>
  );
}

function Placeholder() {
  return (
    <motion.div initial={{ opacity: 0, y: 8 }} animate={{ opacity: 1, y: 0 }}>
      <Card className="p-8 sm:p-10 text-center">
        <div className="font-display text-2xl sm:text-3xl font-semibold mb-3">
          Listos para calcular
        </div>
        <p className="text-ink/60 max-w-md mx-auto text-sm sm:text-base">
          Rellena la ficha de la izquierda y pulsa <strong>Calcular</strong>. Aquí aparecerán tus
          calorías, macros, IMC y un plan adaptado a tu objetivo.
        </p>
        <div className="mt-7 grid grid-cols-3 gap-2 sm:gap-3 text-[10px] sm:text-xs font-mono uppercase tracking-wider text-ink/40">
          <div className="border border-dashed border-ink/30 py-3 sm:py-4">tmb · get</div>
          <div className="border border-dashed border-ink/30 py-3 sm:py-4">macros</div>
          <div className="border border-dashed border-ink/30 py-3 sm:py-4">plan</div>
        </div>
      </Card>
    </motion.div>
  );
}

function CaloriasHero({ data }: { data: CalculoOutput }) {
  return (
    <Card className="bg-ink text-sand p-5 sm:p-7 relative overflow-hidden">
      <div className="flex items-start sm:items-baseline justify-between gap-3 flex-col sm:flex-row">
        <div>
          <div className="font-mono text-[10px] uppercase tracking-[0.3em] text-sand/60 mb-1">
            calorías recomendadas
          </div>
          <div className="flex items-baseline gap-3">
            <motion.span
              key={data.calorias_recomendadas}
              initial={{ opacity: 0, y: 8 }}
              animate={{ opacity: 1, y: 0 }}
              className="font-display font-semibold leading-none text-ember tabular-nums"
              style={{ fontSize: "clamp(48px, 14vw, 80px)" }}
            >
              {Math.round(data.calorias_recomendadas)}
            </motion.span>
            <span className="font-mono text-xs sm:text-sm text-sand/70">kcal/día</span>
          </div>
        </div>
        <div className="text-left sm:text-right font-mono text-[11px] sm:text-xs uppercase tracking-wider text-sand/60">
          <div>
            {data.tipo_ajuste === "mantener"
              ? "recomposición"
              : `${data.tipo_ajuste} ${data.ajuste_calorico} kcal/día`}
          </div>
          <div className="mt-1">vs GET {data.get}</div>
        </div>
      </div>
    </Card>
  );
}

function StatBox({
  label,
  value,
  unit,
  hint,
  tooltip,
  badge,
}: {
  label: string;
  value: string;
  unit: string;
  hint?: string;
  tooltip?: string;
  badge?: string;
}) {
  const header = tooltip ? (
    <Tooltip>
      <TooltipTrigger asChild>
        <span className="inline-flex items-center gap-1 font-mono text-[10px] uppercase tracking-[0.25em] text-ink/60 mb-2 cursor-help">
          {label} <HelpCircle size={12} strokeWidth={2.25} className="text-ember" />
        </span>
      </TooltipTrigger>
      <TooltipContent>{tooltip}</TooltipContent>
    </Tooltip>
  ) : (
    <div className="font-mono text-[10px] uppercase tracking-[0.25em] text-ink/60 mb-2">
      {label}
    </div>
  );

  return (
    <motion.div whileHover={{ y: -2 }} transition={{ type: "spring", stiffness: 300, damping: 20 }}>
      <Card className="p-4 sm:p-5 relative">
        {badge && (
          <span className="absolute top-2 right-2 font-mono text-[9px] uppercase tracking-wider bg-ochre/30 border border-ochre/60 px-1.5 py-0.5 text-ink/80">
            {badge}
          </span>
        )}
        {header}
        <div className="flex items-baseline gap-2 flex-wrap">
          <span className={`${STAT_NUM} text-3xl sm:text-4xl`}>{value}</span>
          <span className="font-mono text-[11px] sm:text-xs text-ink/60">{unit}</span>
        </div>
        {hint && <div className="font-mono text-[11px] sm:text-xs text-ink/50 mt-1">{hint}</div>}
      </Card>
    </motion.div>
  );
}

function NumField({
  id,
  label,
  value,
  onChange,
  limits,
  suffix,
}: {
  id: string;
  label: string;
  value: number;
  onChange: (v: number) => void;
  limits: { min: number; max: number; step: number };
  suffix?: string;
}) {
  return (
    <div>
      <Label htmlFor={id}>{label}</Label>
      <div className="flex items-stretch border-2 border-ink bg-sand/50 focus-within:bg-surface transition">
        <input
          id={id}
          type="number"
          inputMode="decimal"
          value={Number.isFinite(value) ? value : ""}
          min={limits.min}
          max={limits.max}
          step={limits.step}
          onChange={(e) => onChange(parseFloat(e.target.value))}
          className="flex-1 min-w-0 w-full bg-transparent px-3 py-2.5 font-mono text-base focus:outline-none min-h-[44px]"
        />
        {suffix ? (
          <span className="px-3 self-center font-mono text-[11px] uppercase tracking-wider text-ink/50 border-l border-ink/20">
            {suffix}
          </span>
        ) : null}
      </div>
      <div className="mt-1 font-mono text-[10px] uppercase tracking-wider text-ink/40">
        rango {limits.min}–{limits.max}
      </div>
    </div>
  );
}

function LabelWithHelp({
  children,
  tooltip,
  htmlFor,
}: {
  children: React.ReactNode;
  tooltip: string;
  htmlFor?: string;
}) {
  return (
    <Label htmlFor={htmlFor} className="flex items-center gap-1.5">
      {children}
      <InfoIcon tooltip={tooltip} />
    </Label>
  );
}

function InfoIcon({ tooltip }: { tooltip: string }) {
  return (
    <Tooltip>
      <TooltipTrigger asChild>
        <button
          type="button"
          aria-label="Más información"
          className="inline-flex items-center justify-center text-ember hover:text-clay transition cursor-help"
        >
          <HelpCircle size={14} strokeWidth={2.25} />
        </button>
      </TooltipTrigger>
      <TooltipContent>{tooltip}</TooltipContent>
    </Tooltip>
  );
}

function AgresividadSlider({
  value,
  onChange,
}: {
  value: Agresividad;
  onChange: (v: Agresividad) => void;
}) {
  const idx = Math.max(
    0,
    AGRESIVIDADES.findIndex((a) => a.id === value),
  );
  const max = AGRESIVIDADES.length - 1;
  const current = AGRESIVIDADES[idx];
  return (
    <div className="border-2 border-ink bg-sand/40 p-4">
      <div className="flex items-baseline justify-between mb-3">
        <span className="font-mono text-sm uppercase tracking-wider text-ink">{current.label}</span>
        <span className="font-mono text-xs text-ink/60">{current.nota}</span>
      </div>
      <input
        type="range"
        min={0}
        max={max}
        step={1}
        value={idx}
        onChange={(e) => onChange(AGRESIVIDADES[Number(e.target.value)].id)}
        aria-label="Ritmo del cambio"
        className="w-full appearance-none bg-transparent cursor-pointer focus:outline-none kcals-range"
      />
    </div>
  );
}

function EnfoqueRecompSelector({
  value,
  onChange,
}: {
  value: EnfoqueRecomp;
  onChange: (v: EnfoqueRecomp) => void;
}) {
  const icons: Record<EnfoqueRecomp, typeof Dumbbell> = {
    musculo: Dumbbell,
    equilibrio: Scale,
    grasa: Flame,
  };
  return (
    <div className="grid grid-cols-3 gap-2">
      {ENFOQUES_RECOMP.map((o) => {
        const Icon = icons[o.id];
        const active = value === o.id;
        return (
          <button
            key={o.id}
            type="button"
            onClick={() => onChange(o.id)}
            aria-pressed={active}
            className={
              "flex flex-col items-center justify-center gap-1.5 border-2 border-ink py-3 px-2 transition " +
              (active
                ? "bg-ember text-white shadow-notchSm"
                : "bg-surface text-ink hover:bg-sand/60")
            }
          >
            <Icon size={26} strokeWidth={1.75} />
            <span className="font-mono text-[11px] uppercase tracking-wider text-center leading-tight">
              {o.label}
            </span>
            <span
              className={
                "font-mono text-[9px] text-center leading-tight " +
                (active ? "text-white/80" : "text-ink/55")
              }
            >
              {o.hint}
            </span>
          </button>
        );
      })}
    </div>
  );
}
