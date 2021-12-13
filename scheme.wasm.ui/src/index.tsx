import * as React from "react";
import * as ReactDOM from "react-dom";
import { Burger } from "./components/Burger";
import { useOnClickOutside } from "./components/hooks";
import { SettingsMenu } from "./components/SettingsMenu";
import { Terminal } from "./components/Terminal";
import {
  EditorThemeProvider,
  ThemeContext,
  ThemeProvider,
} from "./components/ThemeProvider";
import { kSolarizedDark, kSolarizedLight } from "./monaco/solarized";
import { SchemeRuntime } from "./SchemeRuntime";

interface AppState {
  theme: string;
  editorTheme: string;
  open: boolean;
  stopped: boolean;
}

const kDefaultState: AppState = {
  theme: "Dark",
  editorTheme: "Same",
  open: false,
  stopped: false,
};

const App: React.FunctionComponent<{}> = (props) => {
  const [state, setState] = React.useState<AppState>(kDefaultState);
  const runtime = React.useRef<SchemeRuntime>();
  const ref = React.useRef<HTMLDivElement>(null);

  useOnClickOutside(ref, () => setState({ ...state, open: false }));

  React.useEffect(() => {
    SchemeRuntime.load()
      .then((schemeRuntime) => {
        console.log("Loaded");
        runtime.current = schemeRuntime;
        setState({ ...state, stopped: false });
      })
      .catch((err) => console.error(err));
  }, ["once"]);

  const onInput = (str: string): string => {
    if (!runtime.current || runtime.current.stopped) {
      runtime.current?.start();
      setTimeout(() => setState({ ...state, stopped: false }), 100);
      return "\x1B[0;94mRestarted.\x1B[0m ";
    }
    const result = runtime.current.processLine(str);
    if (runtime.current.stopped) {
      setTimeout(() => setState({ ...state, stopped: true }), 100);
    }
    return result;
  };

  const getEditorTheme = () => {
    if (state.editorTheme == "Same") {
      return false;
    } else if (state.editorTheme == "Dark") {
      return kSolarizedDark;
    } else {
      return kSolarizedLight;
    }
  };

  return (
    <ThemeProvider
      value={state.theme == "Dark" ? kSolarizedDark : kSolarizedLight}
    >
      <EditorThemeProvider value={getEditorTheme()}>
        <div
          style={{
            display: "grid",
            height: "95vh",
            width: "90vw",
            margin: "0 auto",
            boxShadow: "#444 0 0.5em 1em",
            overflowY: "scroll",
          }}
        >
          <Terminal
            prompt={state.stopped ? "stopped:" : "> "}
            pause={state.stopped}
            welcomeMessage="Welcome to scheme.wasm"
            autofocus={!state.open}
            onInput={(str) => onInput(str)}
          />
        </div>
        <div ref={ref}>
          <Burger
            open={state.open}
            onClick={() => setState({ ...state, open: !state.open })}
          />
          <SettingsMenu open={state.open}>
            <div
              style={{
                fontWeight: "bolder",
                fontSize: "larger",
                margin: "1rem 0",
              }}
            >
              Settings
            </div>
            <div style={{ lineHeight: "2em" }}>
              REPL Theme:{" "}
              <select
                style={{
                  fontSize: "inherit",
                  fontFamily: "inherit",
                  width: "10em",
                  height: "1.5em",
                  borderRadius: "0.25em",
                  appearance: "none",
                  padding: "0 0.25em",
                }}
                value={state.theme}
                onChange={(e) => {
                  setState({ ...state, theme: e.target.value });
                }}
              >
                <option value="Dark">Dark</option>
                <option value="Light">Light</option>
              </select>
            </div>
            <div style={{ lineHeight: "2em" }}>
              Editor Theme:{" "}
              <select
                style={{
                  fontSize: "inherit",
                  fontFamily: "inherit",
                  width: "10em",
                  height: "1.5em",
                  borderRadius: "0.25em",
                  appearance: "none",
                  padding: "0 0.25em",
                }}
                value={state.editorTheme}
                onChange={(e) => {
                  setState({ ...state, editorTheme: e.target.value });
                }}
              >
                <option value="Same">Same as REPL</option>
                <option value="Dark">Dark</option>
                <option value="Light">Light</option>
              </select>
            </div>
          </SettingsMenu>
        </div>
      </EditorThemeProvider>
    </ThemeProvider>
  );
};

ReactDOM.render(<App />, document.getElementById("app"));
