import type { CalculoInput, CalculoOutput } from "./types";

const BASE = "/api";

export async function calcular(input: CalculoInput): Promise<CalculoOutput> {
  const res = await fetch(`${BASE}/plan`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(input),
  });
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
}
