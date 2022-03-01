import React from "react";
import Prism from "prismjs";
import { DebugBreakEvent } from "../RuntimeWorker";
import { SchemeRuntimeContext } from "./SchemeRuntimeProvider";
import { ThemeContext } from "./ThemeProvider";
import { ToggleSwitch } from "./ToggleSwitch";
import { EnvResponse } from "../worker/messages";
import { RuntimeStatus } from "./RuntimeStatus";
import { FocusContext } from "./FocusContext";

const kEmptyDebug: DebugBreakEvent = {
  ptr: 0,
  env: 0,
  expr: "",
  step: (resultStep: boolean) => {},
};

const kButtonStyle: React.CSSProperties = {
  fontSize: "inherit",
  minWidth: "7em",
  minHeight: "1.5em",
  margin: "0.25em",
  border: `1px solid`,
  borderRadius: "0.25em",
};

export const Debugger: React.FunctionComponent = (props) => {
  const runtime = React.useContext(SchemeRuntimeContext);
  const theme = React.useContext(ThemeContext);
  const focus = React.useContext(FocusContext);
  const [enabled, setEnabled] = React.useState(false);
  const [debugInfo, setDebugInfo] =
    React.useState<DebugBreakEvent>(kEmptyDebug);
  const [environments, setEnvironments] = React.useState<EnvResponse[]>([]);
  const history = React.useRef<string[]>([]);
  const expressions = React.useRef<Map<number, string>>(new Map());
  const [original, setOriginal] = React.useState("");
  const waitFor = React.useRef({ ptr: 0 });

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
      history.current = [];
      waitFor.current.ptr = 0;
      expressions.current.clear();
    }
  };

  const addHistory = (str: string) => {
    history.current.unshift(genPrismString(str));
  };

  const onDebugInfo = async (evt: DebugBreakEvent) => {
    if (waitFor.current.ptr !== 0) {
      if (evt.ptr !== waitFor.current.ptr) {
        evt.step(false);
        return;
      } else {
        waitFor.current.ptr = 0;
      }
    }
    setDebugInfo(evt);
    if (expressions.current.has(evt.ptr)) {
      const updatedOriginal = expressions.current.get(evt.ptr) as string;
      setOriginal(updatedOriginal);
      addHistory(`${evt.expr} ⇐ ${updatedOriginal}`);
    } else {
      expressions.current.set(evt.ptr, evt.expr);
      setOriginal("");
      addHistory(evt.expr);
    }
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

  const buttonStyle = (): React.CSSProperties => {
    return {
      ...kButtonStyle,
      background: theme.blue,
      borderColor: theme.base00,
      color: theme.foreground,
      opacity: debugInfo.ptr == 0 ? 0.5 : undefined,
    };
  };

  if (!runtime) {
    <div style={{ margin: "1em", color: theme.base00 }}>
      Runtime not available
    </div>;
  }

  const genPrismString = (str: string) =>
    Prism.highlight(str, Prism.languages.scheme, "scheme");

  return (
    <div style={{ padding: "0.5em" }}>
      <div style={{ display: "grid", gridTemplateColumns: "auto 1fr auto" }}>
        <div style={{ gridColumnStart: 1 }}>
          Debugging:{" "}
          <ToggleSwitch
            disabled={!focus}
            on={enabled}
            onChange={() => {
              runtime?.enableDebug(!enabled);
              setEnabled(!enabled);
            }}
          />
        </div>
        <div style={{ lineHeight: "2em", gridColumnStart: 1 }}>
          <span style={{ fontWeight: "bolder" }}>Ptr:</span> 0x
          {debugInfo.ptr.toString(16)}
        </div>
        <div style={{ gridColumnStart: 3, gridRowStart: 1 }}>
          <RuntimeStatus disabled={!focus} />
        </div>
      </div>
      <div>
        <div
          style={{
            whiteSpace: "pre",
            fontFamily: "'Source Code Pro', monospace",
            margin: "0.5em 0",
            width: "30em",
            height: "10em",
            overflowX: "hidden",
            overflowY: "scroll",
            display: "flex",
            flexDirection: "column-reverse",
            border: `solid 1px ${theme.base00}`,
            background: theme.background,
          }}
          tabIndex={focus ? 0 : -1}
          title="History of evaluated expreessions"
        >
          {history.current
            .filter((el, idx) => idx > 0)
            .map((el, idx) => (
              <div key={idx} dangerouslySetInnerHTML={{ __html: el }} />
            ))}
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
            overflowY: "scroll",
            border: `solid 1px ${theme.base00}`,
            background: theme.background,
          }}
          title="Current expression being evaluated"
          tabIndex={focus ? 0 : -1}
          dangerouslySetInnerHTML={
            debugInfo.ptr
              ? {
                  __html: genPrismString(
                    debugInfo.expr + (original.length ? ` ⇐ ${original}` : "")
                  ),
                }
              : undefined
          }
        />
        <div>
          <button
            style={buttonStyle()}
            disabled={!focus || debugInfo.ptr == 0}
            onClick={() => debugInfo.step(false)}
            title="Evaluate the current expression"
          >
            Step
          </button>
          <button
            style={buttonStyle()}
            disabled={!focus || debugInfo.ptr == 0}
            onClick={() => debugInfo.step(true)}
            title="Evaluate the current expression, show the result when it is available"
          >
            Step w/Result
          </button>
          <button
            style={buttonStyle()}
            disabled={!focus || debugInfo.ptr == 0 || original.length > 0}
            onClick={() => {
              waitFor.current.ptr = debugInfo.ptr;
              debugInfo.step(true);
            }}
            title="Evaluate the current expression and run until it returns a result"
          >
            Run to Result
          </button>
          <button
            style={{
              ...buttonStyle(),
              backgroundColor:
                waitFor.current.ptr === -1 ? theme.red : theme.blue,
            }}
            disabled={!focus || !enabled || !runtime || !runtime.waiting}
            onClick={() => {
              if (waitFor.current.ptr === -1) {
                waitFor.current.ptr = 0;
              } else {
                waitFor.current.ptr = -1;
                debugInfo.step(true);
              }
            }}
            title={
              waitFor.current.ptr === -1
                ? "Interrupt Execution"
                : "Run until idle"
            }
          >
            {waitFor.current.ptr === -1 ? "Break" : "Run"}
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
                background: theme.background,
              }}
              tabIndex={focus ? 0 : -1}
              title={`Environment 0x${el.ptr.toString(16)}`}
            >
              {el.entries
                .filter(
                  (entry) =>
                    el.next || !entry.value.match(/^<(bui|spe|syn|lam)/)
                )
                .map((entry) => [
                  <div
                    style={{
                      borderRight: `solid 0.25em ${theme.boldBackground}`,
                    }}
                    id={`${el.ptr}.n.${entry.name}`}
                  >
                    {entry.name}
                  </div>,
                  <div id={`${el.ptr}.v.${entry.name}`}>{entry.value}</div>,
                ])}
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};
