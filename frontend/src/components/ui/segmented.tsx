import * as React from "react";

import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";

interface SegmentedItem<T extends string> {
  id: T;
  label: React.ReactNode;
  hint?: React.ReactNode;
  title?: string;
}

interface SegmentedProps<T extends string> {
  value: T;
  onChange: (v: T) => void;
  items: readonly SegmentedItem<T>[];
  columns?: number;
  activeTone?: "dark" | "ember";
  className?: string;
  renderItem?: (item: SegmentedItem<T>, active: boolean) => React.ReactNode;
}

export function Segmented<T extends string>({
  value,
  onChange,
  items,
  columns,
  activeTone = "dark",
  className,
  renderItem,
}: SegmentedProps<T>) {
  const cols = columns ?? items.length;
  return (
    <div
      className={cn("grid border-2 border-ink", className)}
      style={{ gridTemplateColumns: `repeat(${cols}, minmax(0, 1fr))` }}
      role="radiogroup"
    >
      {items.map((it, i) => {
        const active = value === it.id;
        const variant = active
          ? activeTone === "ember"
            ? "segmentActiveEmber"
            : "segmentActiveDark"
          : "segment";
        return (
          <Button
            key={it.id}
            type="button"
            variant={variant}
            size="seg"
            role="radio"
            aria-checked={active}
            title={it.title}
            onClick={() => onChange(it.id)}
            className={cn(
              "rounded-none",
              i < items.length - 1 && i % cols !== cols - 1 && "border-r-2 border-ink",
            )}
          >
            {renderItem ? renderItem(it, active) : it.label}
          </Button>
        );
      })}
    </div>
  );
}
