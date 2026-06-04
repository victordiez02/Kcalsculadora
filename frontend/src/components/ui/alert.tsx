import * as React from "react";
import { cva, type VariantProps } from "class-variance-authority";

import { cn } from "@/lib/utils";

const alertVariants = cva(
  "relative w-full border-2 px-3 sm:px-4 py-3 flex items-start gap-3 [&>svg]:shrink-0 [&>svg]:mt-0.5",
  {
    variants: {
      variant: {
        info: "bg-sky/10 border-sky text-sky",
        warning: "bg-ochre/15 border-ochre text-clay",
        danger: "bg-ember/15 border-ember text-ember",
      },
    },
    defaultVariants: { variant: "info" },
  },
);

const Alert = React.forwardRef<
  HTMLDivElement,
  React.HTMLAttributes<HTMLDivElement> & VariantProps<typeof alertVariants>
>(({ className, variant, ...props }, ref) => (
  <div ref={ref} role="alert" className={cn(alertVariants({ variant }), className)} {...props} />
));
Alert.displayName = "Alert";

const AlertDescription = React.forwardRef<
  HTMLParagraphElement,
  React.HTMLAttributes<HTMLParagraphElement>
>(({ className, ...props }, ref) => (
  <p ref={ref} className={cn("text-sm leading-snug text-ink", className)} {...props} />
));
AlertDescription.displayName = "AlertDescription";

// eslint-disable-next-line react-refresh/only-export-components
export { Alert, AlertDescription, alertVariants };
