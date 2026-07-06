import path from "node:path";
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
  server: {
    port: 5173,
    host: true,
    proxy: {
      "/api": {
        target: process.env.VITE_BACKEND_PROXY_TARGET || "http://localhost:8000",
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ""),
      },
      "/diet-api": {
        target: process.env.VITE_DIET_BACKEND_PROXY_TARGET || "http://localhost:8001",
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/diet-api/, ""),
      },
    },
    watch: process.env.VITE_USE_POLLING === "true" ? { usePolling: true } : undefined,
  },
});
