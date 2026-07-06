/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_API_URL: string;
  // Añade aquí más variables si las necesitas en el futuro
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
