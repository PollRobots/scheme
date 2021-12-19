import * as React from "react";
import * as ReactDOM from "react-dom";
import { About } from "./components/About";
import { Burger } from "./components/Burger";
import { Flyout } from "./components/Flyout";
import { HeapInspector } from "./components/HeapInspector";
import { useOnClickOutside } from "./components/hooks";
import { SchemeRuntimeProvider } from "./components/SchemeRuntimeProvider";
import { Settings } from "./components/Settings";
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
  about: boolean;
  first: boolean;
  inspector: boolean;
}

const kDefaultState: AppState = {
  theme: "Dark",
  editorTheme: "Same",
  open: false,
  about: false,
  stopped: true,
  first: true,
  inspector: true,
};

const kSettingsSubHeading: React.CSSProperties = {
  fontSize: "1.25em",
  lineHeight: "2em",
};

const App: React.FunctionComponent<{}> = (props) => {
  const [state, setState] = React.useState<AppState>(kDefaultState);
  const runtime = React.useRef<SchemeRuntime>();
  const ref = React.useRef<HTMLDivElement>(null);

  useOnClickOutside(ref, () =>
    setState({ ...state, open: false, about: false })
  );

  React.useEffect(() => {
    // SchemeRuntime.load()
    //   .then((schemeRuntime) => {
    //     console.log("Loaded");
    //     runtime.current = schemeRuntime;
    //     setState({ ...state, stopped: false });
    //   })
    //   .catch((err) => {
    //     if (err instanceof Error) {
    //       setState({ ...state, err: err });
    //     } else {
    //       console.error(err);
    //     }
    //   });
  }, ["once"]);

  const onInput = async (str: string): Promise<string> => {
    try {
      if (!runtime.current) {
        runtime.current = await SchemeRuntime.load();
        setTimeout(
          () => setState({ ...state, first: false, stopped: false }),
          100
        );
        return "\x1B[0;94mStarted runtime.\x1B[0m ";
      } else if (!runtime.current || runtime.current.stopped) {
        await runtime.current?.start();
        setTimeout(() => setState({ ...state, stopped: false }), 100);
        return "\x1B[0;94mRestarted.\x1B[0m ";
      }
      const result = runtime.current.processLine(str);
      if (runtime.current.stopped) {
        setTimeout(() => setState({ ...state, stopped: true }), 100);
      }
      return result;
    } catch (err) {
      return `\x1B[0;31m${err}

\x1B[0;94mIt's unlikely that trying again will help ‾\\_(シ)_/‾ \x1B[0m
`;
    }
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
      <SchemeRuntimeProvider value={runtime.current}>
        <EditorThemeProvider value={getEditorTheme()}>
          <div
            style={{
              display: "grid",
              height: "calc(100vh - 5rem)",
              width: "calc(95vw - 7rem)",
              margin: "1rem auto 4rem",
              boxSizing: "border-box",
              boxShadow: "#444 0 0.5em 1em",
              overflowY: "scroll",
            }}
          >
            <Terminal
              prompt={
                state.first ? "start:" : state.stopped ? "stopped:" : "> "
              }
              pause={state.stopped}
              welcomeMessage="Welcome to scheme.wasm"
              autofocus={!state.open && !state.inspector}
              onInput={(str) => onInput(str)}
            />
          </div>
          {state.inspector ? (
            <Flyout label="Inspector">
              <HeapInspector />
            </Flyout>
          ) : null}

          <div ref={ref}>
            <Burger
              open={state.open}
              onClick={() => setState({ ...state, open: !state.open })}
            />
            <SettingsMenu open={state.open}>
              <Settings
                theme={state.theme}
                editorTheme={state.editorTheme}
                inspector={state.inspector}
                onChange={(update) => setState({ ...state, ...update })}
                onAbout={() => setState({ ...state, about: true, open: false })}
              />
            </SettingsMenu>
          </div>
          {state.about ? <About /> : null}
        </EditorThemeProvider>
      </SchemeRuntimeProvider>
    </ThemeProvider>
  );
};

ReactDOM.render(<App />, document.getElementById("app"));
