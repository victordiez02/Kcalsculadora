import type { DietInput, DietOutput } from "@/types";

// Segundo backend (backend-diet). En dev, el proxy de Vite redirige
// /diet-api -> http://localhost:8001.
const BASE_URL = import.meta.env.VITE_DIET_API_URL || "/diet-api";

/**
 * Despierta el backend de dietas (Cloud Run puede tener cold start).
 * Se llama al abrir el wizard para que la generación no pague el arranque.
 */
export function despertarDietApi() {
  fetch(`${BASE_URL}/health`).catch(() => {
    // solo es un warm-up: los errores se ignoran
  });
}

/**
 * Genera el plan de comidas. Es una llamada larga (varios pasos de LLM),
 * así que no se reintenta automáticamente: el usuario decide.
 */
export async function generarDieta(input: DietInput): Promise<DietOutput> {
  let res: Response;
  try {
    res = await fetch(`${BASE_URL}/diet/generate`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(input),
    });
  } catch {
    throw new Error("No se pudo conectar con el generador de dietas. Inténtalo de nuevo.");
  }

  if (!res.ok) {
    let mensaje = `Error ${res.status}`;
    try {
      const data = await res.json();
      if (typeof data?.detail === "string") {
        mensaje = data.detail;
      } else if (data?.detalle && Array.isArray(data.detalle)) {
        mensaje = data.detalle
          .map((d: { campo: string; error: string }) => `${d.campo}: ${d.error}`)
          .join(" · ");
      }
    } catch {
      // deja el mensaje genérico
    }
    throw new Error(mensaje);
  }

  return (await res.json()) as DietOutput;
}

/** Convierte el PDF en base64 de la respuesta en una descarga del navegador. */
export function descargarPdf(pdfBase64: string, nombreFichero = "plan-comidas.pdf") {
  const bytes = Uint8Array.from(atob(pdfBase64), (c) => c.charCodeAt(0));
  const blob = new Blob([bytes], { type: "application/pdf" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = nombreFichero;
  a.click();
  URL.revokeObjectURL(url);
}
