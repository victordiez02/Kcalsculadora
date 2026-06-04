import animate from "tailwindcss-animate";

/** @type {import('tailwindcss').Config} */
export default {
  darkMode: "class",
  content: ["./index.html", "./src/**/*.{ts,tsx}"],
  theme: {
    extend: {
      colors: {
        // Tokens en CSS variables (rgb triplet) para soportar light/dark sin tocar componentes.
        sand: "rgb(var(--sand) / <alpha-value>)",
        sandDark: "rgb(var(--sand-dark) / <alpha-value>)",
        ink: "rgb(var(--ink) / <alpha-value>)",
        line: "rgb(var(--ink) / <alpha-value>)",
        surface: "rgb(var(--surface) / <alpha-value>)",
        ember: "rgb(var(--ember) / <alpha-value>)",
        moss: "rgb(var(--moss) / <alpha-value>)",
        ochre: "rgb(var(--ochre) / <alpha-value>)",
        clay: "rgb(var(--clay) / <alpha-value>)",
        sky: "rgb(var(--sky) / <alpha-value>)",
      },
      fontFamily: {
        display: ['"Fraunces"', "serif"],
        body: ['"Inter"', "system-ui", "sans-serif"],
        mono: ['"JetBrains Mono"', "ui-monospace", "monospace"],
      },
      boxShadow: {
        notch: "4px 4px 0 0 rgb(var(--ink) / 1)",
        notchSm: "2px 2px 0 0 rgb(var(--ink) / 1)",
      },
      keyframes: {
        "accordion-down": {
          from: { height: "0" },
          to: { height: "var(--radix-accordion-content-height)" },
        },
        "accordion-up": {
          from: { height: "var(--radix-accordion-content-height)" },
          to: { height: "0" },
        },
      },
      animation: {
        "accordion-down": "accordion-down 0.2s ease-out",
        "accordion-up": "accordion-up 0.2s ease-out",
      },
    },
  },
  plugins: [animate],
};
