import * as React from "react";

import { cn } from "@/lib/utils";

export type InputProps = React.InputHTMLAttributes<HTMLInputElement>;

const Input = React.forwardRef<HTMLInputElement, InputProps>(
  ({ className, type, ...props }, ref) => (
    <input
      ref={ref}
      type={type}
      className={cn(
        "w-full bg-sand/50 border-2 border-ink px-3 py-2.5 font-mono text-base",
        "transition min-h-[44px]",
        "focus:outline-none focus:bg-surface focus:ring-0",
        "disabled:opacity-60 disabled:cursor-not-allowed",
        className,
      )}
      {...props}
    />
  ),
);
Input.displayName = "Input";

export { Input };
