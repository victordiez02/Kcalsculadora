export type Sexo = "M" | "F";
export type Nivel = "principiante" | "intermedio" | "avanzado";
export type Objetivo = "mant" | "def" | "vol";
export type Agresividad = "muy_baja" | "baja" | "moderada" | "alta" | "muy_alta";
export type EnfoqueRecomp = "musculo" | "equilibrio" | "grasa";

export interface CalculoInput {
  peso: number;
  altura: number;
  edad: number;
  sexo: Sexo;
  grasa: number | null;
  nivel: Nivel;
  actividad: number;
  objetivo: Objetivo;
  agresividad: Agresividad;
  enfoque_recomp?: EnfoqueRecomp;
}

export interface Macros {
  proteinas: number;
  grasas: number;
  carbohidratos: number;
}

export interface Aviso {
  nivel: "info" | "warning" | "danger";
  mensaje: string;
}

export interface AjusteRecomendado {
  calorias: number;
  macros: Macros;
  semanas_minimas: number;
  delta_kcal_dia: number;
  gramos_por_semana: number;
  motivo: string;
}

export interface CalculoOutput {
  imc: number;
  imc_categoria: string;
  tmb: number;
  get: number;
  masa_magra: number;
  grasa_estimada: number;
  grasa_es_estimada: boolean;
  peso_objetivo: number;
  calorias_recomendadas: number;
  macros: Macros;
  semanas_necesarias: number | null;
  semanas_minimas: number;
  ajuste_calorico: number;
  tipo_ajuste: "reducir" | "aumentar" | "mantener";
  ganancia_muscular_kg: number;
  perdida_grasa_kg: number;
  grasa_objetivo: number;
  avisos: Aviso[];
  ajuste_recomendado: AjusteRecomendado | null;
  factor_actividad: number;
  detalle: Detalle;
}

export interface Detalle {
  tmb_coef_peso: number;
  tmb_coef_altura: number;
  tmb_coef_edad: number;
  tmb_offset: number;
  grasa_coef_imc: number;
  grasa_coef_edad: number;
  grasa_coef_sexo: number;
  grasa_const: number;
  grasa_sexo_val: number;
  grasa_edad_efectiva: number;
  grasa_ajuste_nivel: number;
  proteina_g_kg: number;
  grasa_g_kg: number;
  kcal_por_g_proteina: number;
  kcal_por_g_grasa: number;
  kcal_por_g_carbo: number;
  agresividad_valor: number | null;
  deficit_rango: [number, number] | null;
  superavit_rango: [number, number] | null;
  superavit_factor: number | null;
  recomp_pct_get: number | null;
  recomp_ajuste_max: number | null;
  recomp_ganancia_factor: number | null;
  recomp_perdida_grasa_factor: number | null;
  ganancia_base_mensual: number;
  ganancia_aj_agresividad: number;
  ganancia_aj_actividad: number;
  ganancia_factor_sexo: number;
  ganancia_factor_edad: number;
  ganancia_semanal_kg: number;
  kcal_por_kg_grasa: number;
  suavizado_semanas: number;
  ajuste_duracion: number | null;
}
