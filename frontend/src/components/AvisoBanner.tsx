import { motion } from "framer-motion";
import { AlertTriangle, Info, XCircle, type LucideIcon } from "lucide-react";

import { Alert, AlertDescription } from "@/components/ui/alert";
import type { Aviso } from "@/types";

const ICONS: Record<Aviso["nivel"], LucideIcon> = {
  info: Info,
  warning: AlertTriangle,
  danger: XCircle,
};

export function AvisoBanner({ aviso }: { aviso: Aviso }) {
  const Icon = ICONS[aviso.nivel];
  return (
    <motion.div initial={{ opacity: 0, x: -8 }} animate={{ opacity: 1, x: 0 }}>
      <Alert variant={aviso.nivel}>
        <Icon size={22} />
        <AlertDescription>{aviso.mensaje}</AlertDescription>
      </Alert>
    </motion.div>
  );
}
