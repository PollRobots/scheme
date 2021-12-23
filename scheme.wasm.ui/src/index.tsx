import React from "react";
import ReactDOM from "react-dom";
import { About } from "./components/About";
import { Burger } from "./components/Burger";
import { Flyout } from "./components/Flyout";
import { HeapInspector } from "./components/HeapInspector";
import { useOnClickOutside } from "./components/hooks";
import { SchemeRuntimeProvider } from "./components/SchemeRuntimeProvider";
import { Settings, SettingsBase } from "./components/Settings";
import { SettingsMenu } from "./components/SettingsMenu";
import { Terminal } from "./components/Terminal";
import { EditorThemeProvider, ThemeProvider } from "./components/ThemeProvider";
import { kSolarizedDark, kSolarizedLight } from "./monaco/solarized";
import { SchemeRuntime } from "./SchemeRuntime";
import { reference } from "./util";

import fonts from "./styles/fonts.module.css";
import css from "./styles/page.module.css";

reference(fonts, css);

interface AppState {
  theme: string;
  editorTheme: string;
  fontSize: number;
  open: boolean;
  stopped: boolean;
  about: boolean;
  first: boolean;
  inspector: boolean;
  persist: boolean;
}

const kDefaultState: AppState = {
  theme: "Dark",
  editorTheme: "Same",
  open: false,
  about: false,
  stopped: true,
  first: true,
  inspector: false,
  persist: false,
  fontSize: 12,
};

const kSettingsSubHeading: React.CSSProperties = {
  fontSize: "1.25em",
  lineHeight: "2em",
};

const kSettingsKey = "scheme.wasm.ui:settings";

function storeSettings(settings: SettingsBase) {
  const data: SettingsBase = {
    theme: settings.theme,
    editorTheme: settings.editorTheme,
    fontSize: settings.fontSize,
    inspector: settings.inspector,
    persist: true,
  };

  localStorage.setItem(kSettingsKey, JSON.stringify(data));
}

function loadSettings(): Partial<SettingsBase> {
  const value = localStorage.getItem(kSettingsKey);
  const settings: Partial<SettingsBase> = { persist: false };
  if (value) {
    const data = JSON.parse(value);
    if (data && typeof data === "object") {
      if (
        Reflect.has(data, "theme") &&
        typeof data.theme === "string" &&
        (data.theme === "Dark" || data.theme === "Light")
      ) {
        settings.theme = data.theme;
      }
      if (
        Reflect.has(data, "editorTheme") &&
        typeof data.editorTheme === "string" &&
        (data.editorTheme === "Dark" ||
          data.editorTheme === "Light" ||
          data.editorTheme === "Same")
      ) {
        settings.editorTheme = data.editorTheme;
      }
      if (
        Reflect.has(data, "fontSize") &&
        typeof data.fontSize === "number" &&
        data.fontSize >= 6 &&
        data.fontSize <= 96
      ) {
        settings.fontSize = data.fontSize;
      }
      if (
        Reflect.has(data, "inspector") &&
        typeof data.inspector === "boolean"
      ) {
        settings.inspector = data.inspector;
      }
      settings.persist = true;
    }
  }
  return settings;
}

function clearSettings() {
  localStorage.removeItem(kSettingsKey);
}

const App: React.FunctionComponent<{}> = (props) => {
  const [state, setState] = React.useState<AppState>({
    ...kDefaultState,
    ...loadSettings(),
  });
  const runtime = React.useRef<SchemeRuntime>();
  const ref = React.useRef<HTMLDivElement>(null);

  useOnClickOutside(ref, () =>
    setState({ ...state, open: false, about: false })
  );

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
              fontSize: `${state.fontSize}pt`,
            }}
          >
            <Terminal
              prompt={
                state.first ? "start:" : state.stopped ? "stopped:" : "> "
              }
              pause={state.stopped}
              welcomeMessage="Welcome to scheme.wasm"
              autofocus={!state.open && !state.inspector}
              fontSize={(4 * state.fontSize) / 3}
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
                fontSize={state.fontSize}
                persist={state.persist}
                onChange={(update) => {
                  if (update.persist) {
                    storeSettings(update);
                  } else if (state.persist) {
                    clearSettings();
                  }
                  setState({ ...state, ...update });
                }}
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
