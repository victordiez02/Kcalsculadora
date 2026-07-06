import type { CalculoInput, CalculoOutput } from "./types";

// Usamos la variable de entorno. El fallback es "/api" por si falla la carga del env.
const BASE_URL = import.meta.env.VITE_API_URL || "/api";

/**
 * Función auxiliar para esperar X milisegundos
 */
const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

export async function calcular(input: CalculoInput, intentos = 3): Promise<CalculoOutput> {
  try {
    const res = await fetch(`${BASE_URL}/plan`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(input),
    });

    // Si la API responde con error de servidor (502, 503, 504), es probable que esté arrancando
    if (!res.ok && res.status >= 500 && intentos > 0) {
      await sleep(3000); // Esperamos 3 segundos antes de reintentar
      return calcular(input, intentos - 1);
    }

    if (!res.ok) {
      let mensaje = `Error ${res.status}`;
      try {
        const data = await res.json();
        if (data?.detalle && Array.isArray(data.detalle)) {
          mensaje = data.detalle
            .map((d: { campo: string; error: string }) => `${d.campo}: ${d.error}`)
            .join(" · ");
        } else if (data?.detail) {
          mensaje = typeof data.detail === "string" ? data.detail : JSON.stringify(data.detail);
        }
      } catch {
        mensaje = `${mensaje}: ${await res.text()}`;
      }
      throw new Error(mensaje);
    }

    return (await res.json()) as CalculoOutput;
  } catch (error: unknown) {
    // Si el error es de red (Fetch falló porque la API no responde aún)
    if (intentos > 0) {
      await sleep(4000);
      return calcular(input, intentos - 1);
    }

    // Aquí es donde lanzarías tu Tooltip/Toast en la UI
    // Ejemplo: toast.error(error.message);
    throw error;
  }
}
