import React from "react";

type MouseEventHandler = (event: MouseEvent) => void;

export function useOnClickOutside(
  ref: React.RefObject<HTMLDivElement>,
  handler: MouseEventHandler
) {
  React.useEffect(() => {
    const listener: MouseEventHandler = (event) => {
      const target = event.target;
      if (
        !ref.current ||
        (target instanceof Element && ref.current.contains(target))
      ) {
        return;
      }
      handler(event);
    };
    document.addEventListener("mousedown", listener);
    return () => {
      document.removeEventListener("mousedown", listener);
    };
  }, [ref, handler]);
}
