import React from "react";
import Prism from "prismjs";
import { DebugBreakEvent } from "../RuntimeWorker";
import { SchemeRuntimeContext } from "./SchemeRuntimeProvider";
import { ThemeContext } from "./ThemeProvider";
import { ToggleSwitch } from "./ToggleSwitch";
import { EnvResponse } from "../worker/messages";

const kEmptyDebug: DebugBreakEvent = {
  ptr: 0,
  env: 0,
  expr: "",
  step: () => {},
};

export const Debugger: React.FunctionComponent = (props) => {
  const runtime = React.useContext(SchemeRuntimeContext);
  const theme = React.useContext(ThemeContext);
  const [enabled, setEnabled] = React.useState(false);
  const [debugInfo, setDebugInfo] =
    React.useState<DebugBreakEvent>(kEmptyDebug);
  const [environments, setEnvironments] = React.useState<EnvResponse[]>([]);

  const onStatus = () => {
    if (!runtime) {
      return;
    }
    if (
      runtime.loaded &&
      !runtime.stopped &&
      !runtime.waiting &&
      !runtime.partial
    ) {
      setDebugInfo(kEmptyDebug);
      setEnvironments([]);
    }
  };

  const onDebugInfo = async (evt: DebugBreakEvent) => {
    setDebugInfo(evt);
    if (runtime) {
      let ptr = evt.env;
      let envs: EnvResponse[] = [];
      while (ptr != 0) {
        const response = await runtime.lookupEnv(ptr);
        envs.push(response);
        ptr = response.next;
        setEnvironments([...envs]);
      }
    }
  };

  React.useEffect(() => {
    if (runtime && enabled) {
      runtime.addEventListener("debug", onDebugInfo);
      runtime.addEventListener("status", onStatus);
    }
    return () => {
      if (runtime) {
        runtime.removeEventListener("debug", onDebugInfo);
        runtime.removeEventListener("status", onStatus);
      }
    };
  }, [runtime, enabled]);

  if (!runtime) {
    <div style={{ margin: "1em", color: theme.base00 }}>
      Runtime not available
    </div>;
  }

  return (
    <div style={{ padding: "0.5em" }}>
      <div>
        Debugging:{" "}
        <ToggleSwitch
          on={enabled}
          onChange={() => {
            runtime?.enableDebug(!enabled);
            setEnabled(!enabled);
          }}
        />
      </div>
      <div>
        <div style={{ lineHeight: "2em" }}>
          <span style={{ fontWeight: "bolder" }}>Ptr:</span> 0x
          {debugInfo.ptr.toString(16)}
        </div>
        <div
          style={{
            whiteSpace: "pre-wrap",
            fontFamily: "'Source Code Pro', monospace",
            wordWrap: "break-word",
            wordBreak: "break-all",
            margin: "0.5em 0",
            width: "30em",
            height: "10em",
            overflowY: "visible",
            border: `solid 1px ${theme.base00}`,
            background: theme.background,
          }}
          dangerouslySetInnerHTML={
            debugInfo.ptr
              ? {
                  __html: Prism.highlight(
                    debugInfo.expr,
                    Prism.languages.scheme,
                    "scheme"
                  ),
                }
              : undefined
          }
        />
        <div>
          <button
            style={{
              fontSize: "inherit",
              minWidth: "7em",
              minHeight: "1.5em",
              margin: "0.25em",
              background: theme.blue,
              border: `1px solid ${theme.base00}`,
              borderRadius: "0.25em",
              color: theme.foreground,
              opacity: debugInfo.ptr == 0 ? 0.5 : undefined,
            }}
            disabled={debugInfo.ptr == 0}
            onClick={() => debugInfo.step()}
          >
            Step
          </button>
        </div>
        <div>
          {environments.map((el) => (
            <div
              key={el.ptr}
              style={{
                display: "grid",
                gridTemplateColumns: "8em 1fr",
                columnGap: "0.5em",
                overflowY: "auto",
                maxHeight: "30em",
                margin: "0.5em 0",
                width: "30em",
                border: `solid 1px ${theme.base00}`,
                background: theme.background,
              }}
            >
              {el.entries
                .filter(
                  (entry) =>
                    el.next || !entry.value.match(/^<(bui|spe|syn|lam)/)
                )
                .map((entry) => [
                  <div id={`${el.ptr}.n.${entry.name}`}>{entry.name}</div>,
                  <div id={`${el.ptr}.v.${entry.name}`}>{entry.value}</div>,
                ])}
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};
