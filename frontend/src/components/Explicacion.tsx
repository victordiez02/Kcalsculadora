import { ChevronDown } from "lucide-react";

import { Card } from "@/components/ui/card";
import type { CalculoOutput, Objetivo } from "@/types";

interface Props {
  data: CalculoOutput;
  peso: number;
  altura: number;
  edad: number;
  sexo: "M" | "F";
  actividad: number;
  objetivo: Objetivo;
}

const fmt = (n: number, d = 2) => (Number.isFinite(n) ? n.toFixed(d).replace(/\.?0+$/, "") : "—");

const signed = (n: number) => (n >= 0 ? `+ ${fmt(n)}` : `− ${fmt(Math.abs(n))}`);

export function Explicacion({ data, peso, altura, edad, sexo, actividad, objetivo }: Props) {
  const d = data.detalle;
  const sexoLabel = sexo === "M" ? "hombre" : "mujer";
  const grasaMeta = data.grasa_objetivo;
  const alturaM = altura / 100;
  const kcalProt = Math.round(data.macros.proteinas * d.kcal_por_g_proteina);
  const kcalGra = Math.round(data.macros.grasas * d.kcal_por_g_grasa);
  const kcalCarb = Math.round(data.macros.carbohidratos * d.kcal_por_g_carbo);
  const restoKcal = Math.max(0, data.calorias_recomendadas - kcalProt - kcalGra);

  const objetivoTexto: Record<Objetivo, string> = {
    mant: "recomposición corporal (ganar músculo y perder algo de grasa simultáneamente)",
    def: "definición (perder grasa preservando masa muscular)",
    vol: "volumen (ganar masa muscular asumiendo algo de grasa)",
  };

  return (
    <Card className="p-0 overflow-hidden">
      <details className="group">
        <summary className="list-none cursor-pointer select-none p-5 sm:p-6 flex items-center justify-between gap-3 border-b-2 border-transparent group-open:border-ink/15 transition">
          <div className="flex items-baseline gap-3 min-w-0">
            <span className="font-mono text-[10px] uppercase tracking-[0.3em] text-ember">
              ver el detalle
            </span>
            <h3 className="font-display text-xl sm:text-2xl font-semibold truncate">
              Cómo se obtienen los números
            </h3>
          </div>
          <ChevronDown
            size={22}
            className="shrink-0 text-ink/70 transition-transform group-open:rotate-180"
          />
        </summary>

        <div className="p-5 sm:p-6 space-y-4">
          <p className="text-sm text-ink/75 leading-relaxed">
            Cada bloque muestra primero la fórmula con sus constantes y luego el cálculo con tus
            valores ({sexoLabel}, {edad} años, {altura} cm, {peso} kg) para el objetivo{" "}
            <strong>{objetivoTexto[objetivo]}</strong>. Las constantes que dependen del sexo se
            indican explícitamente.
          </p>

          <Block n="1" title="IMC (Índice de Masa Corporal)">
            <Formula>
              IMC = peso / altura² (m)
              <br />= {peso} / ({alturaM.toFixed(2)})² = {peso} / {(alturaM ** 2).toFixed(4)} ={" "}
              <strong>{data.imc}</strong> · {data.imc_categoria}
            </Formula>
          </Block>

          <Block n="2" title="% de grasa corporal">
            {data.grasa_es_estimada ? (
              <>
                <p className="text-sm text-ink/80 mb-2">
                  Estimación de Deurenberg con corrección por nivel de entrenamiento. El término de
                  sexo vale <strong>1 para hombre y 0 para mujer</strong>; la edad se acota a{" "}
                  {d.grasa_edad_efectiva === edad
                    ? `${edad}`
                    : `${d.grasa_edad_efectiva} (acotada)`}
                  .
                </p>
                <Formula>
                  Grasa = {d.grasa_coef_imc}·IMC + {d.grasa_coef_edad}·edad − {d.grasa_coef_sexo}
                  ·sexo − {d.grasa_const} − ajuste_nivel
                  <br />= {d.grasa_coef_imc}·{data.imc} + {d.grasa_coef_edad}·
                  {d.grasa_edad_efectiva} − {d.grasa_coef_sexo}·{d.grasa_sexo_val} − {d.grasa_const}{" "}
                  − {d.grasa_ajuste_nivel}
                  <br />= <strong>{data.grasa_estimada}%</strong>
                </Formula>
              </>
            ) : (
              <Formula>
                Has indicado <strong>{data.grasa_estimada}%</strong>. Se usa directamente.
              </Formula>
            )}
          </Block>

          <Block n="3" title="Masa magra">
            <Formula>
              Masa magra = peso × (1 − %grasa / 100)
              <br />= {peso} × (1 − {data.grasa_estimada}/100) = {peso} ×{" "}
              {(1 - data.grasa_estimada / 100).toFixed(3)} = <strong>{data.masa_magra} kg</strong>
            </Formula>
          </Block>

          <Block n="4" title="TMB (Mifflin-St Jeor)">
            <p className="text-sm text-ink/80 mb-2">
              El término final depende del sexo:{" "}
              <strong>+{d.tmb_coef_peso === 10 ? "5 si hombre · −161 si mujer" : ""}</strong>. Para
              ti se aplica <strong>{signed(d.tmb_offset)}</strong>.
            </p>
            <Formula>
              TMB = {d.tmb_coef_peso}·peso + {d.tmb_coef_altura}·altura − {d.tmb_coef_edad}·edad{" "}
              {signed(d.tmb_offset)}
              <br />= {d.tmb_coef_peso}·{peso} + {d.tmb_coef_altura}·{altura} − {d.tmb_coef_edad}·
              {edad} {signed(d.tmb_offset)}
              <br />= <strong>{data.tmb} kcal/día</strong>
            </Formula>
          </Block>

          <Block n="5" title="GET (Gasto Energético Total)">
            <Formula>
              GET = TMB × factor de actividad
              <br />= {data.tmb} × {actividad} = <strong>{data.get} kcal/día</strong>
            </Formula>
          </Block>

          <Block n="6" title="Calorías diarias recomendadas">
            {objetivo === "def" && d.deficit_rango && d.agresividad_valor != null && (
              <>
                <p className="text-sm text-ink/80 mb-2">
                  Déficit interpolado en [{d.deficit_rango[0]}, {d.deficit_rango[1]}] kcal/día según
                  agresividad <strong>{d.agresividad_valor}</strong>.
                </p>
                <Formula>
                  déficit = lo + (hi − lo) × (agr − 0.25) / 0.75
                  <br />= {d.deficit_rango[0]} + ({d.deficit_rango[1]} − {d.deficit_rango[0]}) × (
                  {d.agresividad_valor} − 0.25) / 0.75 ={" "}
                  <strong>{Math.round(data.ajuste_calorico)} kcal/día</strong>
                  <br />
                  Calorías = GET − déficit = {data.get} − {Math.round(data.ajuste_calorico)} ={" "}
                  <strong>{data.calorias_recomendadas} kcal/día</strong>
                </Formula>
              </>
            )}
            {objetivo === "vol" && d.superavit_rango && d.superavit_factor != null && (
              <>
                <p className="text-sm text-ink/80 mb-2">
                  Superávit interpolado en [{d.superavit_rango[0]}, {d.superavit_rango[1]}] kcal/día
                  con factor de curva <strong>{d.superavit_factor}</strong> según agresividad.
                </p>
                <Formula>
                  superávit = lo + (hi − lo) × factor
                  <br />= {d.superavit_rango[0]} + ({d.superavit_rango[1]} − {d.superavit_rango[0]})
                  × {d.superavit_factor} ={" "}
                  <strong>{Math.round(data.ajuste_calorico)} kcal/día</strong>
                  <br />
                  Calorías = GET + superávit = {data.get} + {Math.round(data.ajuste_calorico)} ={" "}
                  <strong>{data.calorias_recomendadas} kcal/día</strong>
                </Formula>
              </>
            )}
            {objetivo === "mant" && d.recomp_pct_get != null && (
              <>
                <p className="text-sm text-ink/80 mb-2">
                  Sesgo según enfoque (músculo +5% · equilibrio 0% · grasa −10%), acotado a ±
                  {d.recomp_ajuste_max} kcal/día.
                </p>
                <Formula>
                  ajuste = GET × %_enfoque = {data.get} × {d.recomp_pct_get} ={" "}
                  <strong>{Math.round(data.ajuste_calorico)} kcal/día</strong>
                  <br />
                  {data.tipo_ajuste === "mantener" ? (
                    <>
                      Calorías = GET = {data.get} kcal ={" "}
                      <strong>{data.calorias_recomendadas} kcal/día</strong>
                    </>
                  ) : (
                    <>
                      Calorías = GET {data.tipo_ajuste === "reducir" ? "−" : "+"} ajuste ={" "}
                      {data.get} {data.tipo_ajuste === "reducir" ? "−" : "+"}{" "}
                      {Math.round(data.ajuste_calorico)} ={" "}
                      <strong>{data.calorias_recomendadas} kcal/día</strong>
                    </>
                  )}
                </Formula>
              </>
            )}
          </Block>

          <Block n="7" title="Ganancia muscular semanal">
            <Formula>
              g/sem = (base/4) × aj_agresividad × aj_actividad × f_sexo × f_edad
              <br />= ({d.ganancia_base_mensual} kg/mes ÷ 4) × {d.ganancia_aj_agresividad} ×{" "}
              {d.ganancia_aj_actividad} × {d.ganancia_factor_sexo} (
              {sexo === "M" ? "hombre" : "mujer"}) × {d.ganancia_factor_edad.toFixed(2)} ({edad}{" "}
              años) = <strong>{d.ganancia_semanal_kg} kg/semana</strong>
              {d.recomp_ganancia_factor != null && (
                <> (factor recomp. {d.recomp_ganancia_factor} incluido)</>
              )}
            </Formula>
          </Block>

          <Block n="8" title="Peso objetivo">
            <p className="text-sm text-ink/80 mb-2">
              Se proyecta la masa magra esperada al final del periodo sobre el % de grasa objetivo
              para tu sexo ({sexo === "M" ? "hombre" : "mujer"}: {grasaMeta}%).
            </p>
            <Formula>
              peso objetivo = (masa magra + ganancia muscular) / (1 − grasa objetivo / 100)
              <br />= ({data.masa_magra} + {data.ganancia_muscular_kg}) / (1 − {grasaMeta}/100)
              <br />= {(data.masa_magra + data.ganancia_muscular_kg).toFixed(2)} /{" "}
              {(1 - grasaMeta / 100).toFixed(3)} = <strong>{data.peso_objetivo} kg</strong>
            </Formula>
          </Block>

          {data.semanas_necesarias != null && (
            <Block n="9" title="Duración estimada">
              {objetivo === "mant" ? (
                <Formula>
                  En recomposición el balance calórico es ~0; se fija la duración mínima
                  recomendada: <strong>{data.semanas_minimas} semanas</strong>.
                </Formula>
              ) : (
                <>
                  <p className="text-sm text-ink/80 mb-2">
                    Constantes: <strong>{d.kcal_por_kg_grasa} kcal/kg</strong> (equivalente
                    energético de 1 kg de tejido) y suavizado ×{d.suavizado_semanas} (la fisiología
                    no responde linealmente). Se aplica un ajuste por agresividad de ×
                    {d.ajuste_duracion ?? 1}.
                  </p>
                  <Formula>
                    Δ peso = |{peso} − {data.peso_objetivo}| ={" "}
                    {Math.abs(peso - data.peso_objetivo).toFixed(2)} kg
                    <br />Δ kcal/día = |{data.get} − {data.calorias_recomendadas}| ={" "}
                    {Math.round(data.ajuste_calorico)} kcal
                    <br />
                    semanas = (Δ peso × {d.kcal_por_kg_grasa}) / (Δ kcal × 7 × {d.suavizado_semanas}
                    ) × {d.ajuste_duracion ?? 1}
                    <br />= <strong>{data.semanas_necesarias} semanas</strong> · mínimo recomendado:{" "}
                    {data.semanas_minimas}
                  </Formula>
                </>
              )}
            </Block>
          )}

          {(data.ganancia_muscular_kg > 0 || data.perdida_grasa_kg > 0) && (
            <Block n="10" title="Cambios estimados de composición">
              <Formula>
                {data.ganancia_muscular_kg > 0 && (
                  <>
                    Ganancia muscular ≈ g/sem × semanas (ponderado) ={" "}
                    <strong>{data.ganancia_muscular_kg} kg</strong>
                    <br />
                  </>
                )}
                {data.perdida_grasa_kg > 0 && objetivo === "def" && (
                  <>
                    Pérdida de grasa ≈ peso − peso objetivo ={" "}
                    <strong>{data.perdida_grasa_kg} kg</strong>
                  </>
                )}
                {data.perdida_grasa_kg > 0 &&
                  objetivo === "mant" &&
                  d.recomp_perdida_grasa_factor != null && (
                    <>
                      Pérdida de grasa ≈ (Δ grasa / 100) × peso × factor enfoque
                      <br />= (({data.grasa_estimada} − {grasaMeta}) / 100) × {peso} ×{" "}
                      {d.recomp_perdida_grasa_factor} = <strong>{data.perdida_grasa_kg} kg</strong>
                    </>
                  )}
              </Formula>
            </Block>
          )}

          <Block n="11" title="Macronutrientes">
            <p className="text-sm text-ink/80 mb-2">
              Los <strong>g/kg de proteína y grasa dependen del sexo</strong>: en mujeres se sube la
              proteína en déficit/recomposición y se mantiene un mínimo de grasa para preservar el
              eje hormonal. Los carbohidratos cubren las calorías restantes ({d.kcal_por_g_proteina}{" "}
              kcal/g proteína, {d.kcal_por_g_grasa} kcal/g grasa, {d.kcal_por_g_carbo} kcal/g
              carbohidrato).
            </p>
            <Formula>
              Proteínas = peso × {d.proteina_g_kg} g/kg = {peso} × {d.proteina_g_kg} ={" "}
              <strong>{data.macros.proteinas} g/día</strong> ({kcalProt} kcal)
              <br />
              Grasas = peso × {d.grasa_g_kg} g/kg = {peso} × {d.grasa_g_kg} ={" "}
              <strong>{data.macros.grasas} g/día</strong> ({kcalGra} kcal)
              <br />
              Carbohidratos = (calorías − kcal_prot − kcal_gra) / {d.kcal_por_g_carbo}
              <br />= ({data.calorias_recomendadas} − {kcalProt} − {kcalGra}) / {d.kcal_por_g_carbo}{" "}
              = {restoKcal} / {d.kcal_por_g_carbo} ={" "}
              <strong>{data.macros.carbohidratos} g/día</strong> ({kcalCarb} kcal)
            </Formula>
          </Block>

          <p className="text-xs font-mono text-ink/60 border-t border-ink/10 pt-3">
            Revisa cada 2–4 semanas. Ajusta ±100–200 kcal/día si tu peso, rendimiento o energía se
            desvían de lo esperado.
          </p>
        </div>
      </details>
    </Card>
  );
}

function Block({ n, title, children }: { n: string; title: string; children: React.ReactNode }) {
  return (
    <section className="border-2 border-ink/10 bg-sand/30 p-4">
      <h4 className="font-display text-lg font-semibold mb-2 flex items-center gap-2">
        <span className="inline-flex items-center justify-center w-6 h-6 bg-ember text-white font-mono text-xs">
          {n}
        </span>
        {title}
      </h4>
      {children}
    </section>
  );
}

function Formula({ children }: { children: React.ReactNode }) {
  return (
    <div className="bg-surface border-l-4 border-ember px-4 py-3 font-mono text-sm leading-relaxed whitespace-pre-wrap">
      {children}
    </div>
  );
}
