import React from "react";
import { EmergencyStop } from "./EmergencyStop";
import { SchemeRuntimeContext } from "./SchemeRuntimeProvider";
import { ThemeContext } from "./ThemeProvider";

export const RuntimeStatus: React.FunctionComponent<{ disabled?: boolean }> = (
  props
) => {
  const theme = React.useContext(ThemeContext);
  const runtime = React.useContext(SchemeRuntimeContext);

  const status = () => {
    if (!runtime || !runtime.loaded) {
      return "not started";
    } else if (runtime.stopped) {
      return "stopped";
    } else if (runtime.waiting) {
      return "busy";
    } else if (runtime.partial) {
      return "incomplete input";
    } else {
      return "idle";
    }
  };

  const debugging = () => (runtime && runtime.debugging ? "(debugging)" : null);

  const memsize = (size: number) => {
    if (size < 1024) {
      return `${size} bytes`;
    } else if (size < 0x10_0000) {
      const sizeInKib = size / 1024;
      if (sizeInKib === (sizeInKib | 0)) {
        return `${sizeInKib} KiB`;
      }
      return `${sizeInKib.toFixed(1)} KiB`;
    } else {
      const sizeInMib = size / 0x10_0000;
      if (sizeInMib === (sizeInMib | 0)) {
        return `${sizeInMib} MiB`;
      }
      return `${sizeInMib.toFixed(1)} MiB`;
    }
  };

  const memory = () => {
    if (!runtime || !runtime.loaded) {
      return "n/a";
    } else {
      return `${runtime.memorySize / 0x10000} pages, ${memsize(
        runtime.memorySize
      )}`;
    }
  };

  return (
    <div>
      <div
        style={{
          lineHeight: "2em",
          display: "grid",
          gridTemplateColumns: "1fr auto",
        }}
      >
        Status: {status()}
        {debugging()}
        <EmergencyStop
          msgs={["Stop Runtime", "Confirm"]}
          disabled={
            props.disabled || !runtime || runtime.stopped || !runtime.loaded
          }
          title="This will terminate the scheme runtime worker process."
          style={{
            fontSize: "inherit",
            minWidth: "7em",
            minHeight: "1.5em",
            margin: "0.25em",
            background: theme.blue,
            border: `1px solid ${theme.base00}`,
            borderRadius: "0.25em",
            color: theme.foreground,
          }}
          lastStyle={{
            background: theme.red,
            color: theme.boldForeground,
            fontWeight: "bolder",
          }}
          onAction={() => {
            if (runtime) {
              runtime.terminate();
            }
          }}
        />
      </div>
      <div style={{ lineHeight: "2em" }}>Memory: {memory()}</div>
    </div>
  );
};
