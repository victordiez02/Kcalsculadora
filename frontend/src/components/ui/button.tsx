import * as React from "react";
import { Slot } from "@radix-ui/react-slot";
import { cva, type VariantProps } from "class-variance-authority";

import { cn } from "@/lib/utils";

const buttonVariants = cva(
  "inline-flex items-center justify-center gap-2 border-2 border-ink transition select-none " +
    "active:translate-x-[2px] active:translate-y-[2px] active:shadow-none " +
    "disabled:opacity-60 disabled:cursor-not-allowed disabled:active:translate-x-0 disabled:active:translate-y-0 " +
    "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ink focus-visible:ring-offset-2 focus-visible:ring-offset-sand",
  {
    variants: {
      variant: {
        primary:
          "bg-ember text-white hover:bg-clay font-display font-semibold shadow-notchSm sm:shadow-notch",
        ghost:
          "bg-surface text-ink hover:bg-sand font-mono uppercase tracking-wider shadow-notchSm",
        segment: "bg-surface text-ink hover:bg-sand/60 font-mono uppercase tracking-wider border-0",
        segmentActiveDark: "bg-ink text-sand font-mono uppercase tracking-wider border-0",
        segmentActiveEmber: "bg-ember text-white font-mono uppercase tracking-wider border-0",
      },
      size: {
        md: "px-5 py-3 text-base sm:text-lg min-h-[48px]",
        sm: "px-4 py-2.5 text-sm min-h-[44px]",
        seg: "py-2.5 px-1 text-xs sm:text-sm min-h-[44px]",
      },
    },
    defaultVariants: { variant: "primary", size: "md" },
  },
);

export interface ButtonProps
  extends React.ButtonHTMLAttributes<HTMLButtonElement>, VariantProps<typeof buttonVariants> {
  asChild?: boolean;
}

const Button = React.forwardRef<HTMLButtonElement, ButtonProps>(
  ({ className, variant, size, asChild = false, ...props }, ref) => {
    const Comp = asChild ? Slot : "button";
    return (
      <Comp ref={ref} className={cn(buttonVariants({ variant, size }), className)} {...props} />
    );
  },
);
Button.displayName = "Button";

// eslint-disable-next-line react-refresh/only-export-components
export { Button, buttonVariants };
