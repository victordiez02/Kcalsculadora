/**
 * Catálogos UI (etiquetas y opciones). Los valores numéricos los expone
 * el backend en /config, pero replicamos las opciones aquí para que el
 * formulario pueda renderizar sin red.
 */

export const SEXOS = [
  { id: "M", label: "Masculino" },
  { id: "F", label: "Femenino" },
] as const;

export const NIVELES = [
  { id: "principiante", label: "Principiante", hint: "menos de 6 meses" },
  { id: "intermedio", label: "Intermedio", hint: "6 meses · 2 años" },
  { id: "avanzado", label: "Avanzado", hint: "más de 2 años" },
] as const;

export const OBJETIVOS = [
  { id: "mant", label: "Recomposición" },
  { id: "def", label: "Definición" },
  { id: "vol", label: "Volumen" },
] as const;

export const ENFOQUES_RECOMP = [
  { id: "musculo", label: "Más músculo", hint: "superávit muy ligero" },
  { id: "equilibrio", label: "Equilibrado", hint: "balance neutro" },
  { id: "grasa", label: "Más definición", hint: "déficit muy ligero" },
] as const;

export const AGRESIVIDADES = [
  { id: "muy_baja", label: "muy baja", dot: "◌", nota: "~250 g/sem" },
  { id: "baja", label: "baja", dot: "○", nota: "~400 g/sem" },
  { id: "moderada", label: "moderada", dot: "◐", nota: "~500 g/sem" },
  { id: "alta", label: "alta", dot: "●", nota: "~600 g/sem" },
  { id: "muy_alta", label: "muy alta", dot: "◉", nota: "~650 g/sem" },
] as const;

export const ACTIVIDADES_MIFFLIN = [
  { value: 1.2, label: "Sedentario", hint: "poco o nada de ejercicio" },
  { value: 1.375, label: "Ligera", hint: "1–3 días/semana" },
  { value: 1.55, label: "Moderada", hint: "3–5 días/semana" },
  { value: 1.725, label: "Alta", hint: "6–7 días/semana" },
  { value: 1.9, label: "Muy alta", hint: "entrenos dobles o trabajo físico" },
] as const;

export const ACTIVIDADES_HELMS = [
  {
    value: 1.35,
    label: "Sedentario + entreno",
    hint: "3–6 días entreno fuerza",
  },
  {
    value: 1.55,
    label: "Ligeramente activo + entreno",
    hint: "3–6 días entreno",
  },
  { value: 1.75, label: "Activo + entreno", hint: "3–6 días entreno" },
  { value: 1.95, label: "Muy activo + entreno", hint: "3–6 días entreno" },
] as const;

export const LIMITES = {
  peso: { min: 30, max: 200, step: 0.1 },
  altura: { min: 100, max: 250, step: 1 },
  edad: { min: 14, max: 100, step: 1 },
  grasa: { min: 5, max: 50, step: 0.1 },
} as const;

// ---- Generador de dietas (backend-diet) ----

export const SUPERMERCADOS = [
  { id: "mercadona", label: "Mercadona", color: "#00a650" },
  { id: "carrefour", label: "Carrefour", color: "#004e9f" },
  { id: "lidl", label: "Lidl", color: "#0050aa" },
  { id: "dia", label: "Dia", color: "#d50000" },
] as const;

export const INTOLERANCIAS = [
  { id: "gluten", label: "Gluten" },
  { id: "lactosa", label: "Lácteos" },
  { id: "frutos_secos", label: "Frutos secos" },
  { id: "huevo", label: "Huevo" },
  { id: "pescado", label: "Pescado" },
  { id: "marisco", label: "Marisco" },
  { id: "soja", label: "Soja" },
  { id: "sesamo", label: "Sésamo" },
] as const;

export const NUM_COMIDAS_OPCIONES = [
  { id: "3", label: "3 comidas", hint: "desayuno · comida · cena" },
  { id: "4", label: "4 comidas", hint: "+ merienda" },
  { id: "5", label: "5 comidas", hint: "+ almuerzo y merienda" },
] as const;

export const VARIEDADES = [
  {
    id: "baja",
    label: "Poca variedad",
    hint: "2 menús distintos que se alternan · compra y cocina más fáciles",
  },
  {
    id: "media",
    label: "Variedad media",
    hint: "4 menús distintos repartidos en la semana",
  },
  {
    id: "alta",
    label: "Máxima variedad",
    hint: "un menú diferente para cada día de la semana",
  },
] as const;

export const EVITAR_SUGERENCIAS = [
  "cerdo",
  "ternera",
  "pollo",
  "pavo",
  "atún",
  "salmón",
  "queso",
  "yogur",
  "leche",
  "pan",
  "pasta",
  "arroz",
  "legumbres",
  "tofu",
  "champiñones",
  "tomate",
  "plátano",
  "chocolate",
] as const;

export const FAVORITOS_SUGERENCIAS = [
  "pollo",
  "atún",
  "huevos",
  "yogur griego",
  "skyr",
  "avena",
  "arroz",
  "pasta",
  "pan integral",
  "aguacate",
  "plátano",
  "manzana",
  "queso fresco",
  "salmón",
  "legumbres",
  "frutos secos",
  "chocolate negro",
  "aceite de oliva",
] as const;

export const IMC_BANDAS = [
  { tope: 18.5, label: "Bajo peso", color: "#A9C3DA" },
  { tope: 25, label: "Normal", color: "#A9C99B" },
  { tope: 30, label: "Sobrepeso", color: "#E5C77B" },
  { tope: 35, label: "Obesidad I", color: "#DBA066" },
  { tope: 40, label: "Obesidad II", color: "#CC8258" },
  { tope: 45, label: "Obesidad III", color: "#B85542" },
] as const;
