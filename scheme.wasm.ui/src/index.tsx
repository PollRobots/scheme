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
  prompt: string;
  fontSize: number;
  open: boolean;
  stopped: boolean;
  about: boolean;
  first: boolean;
  inspector: boolean;
  persist: boolean;
  output: string[];
}

const kPartialPrompt = "~    ";
const kRegularPrompt = "> ";

const kDefaultState: AppState = {
  theme: "Dark",
  editorTheme: "Same",
  prompt: kRegularPrompt,
  open: false,
  about: false,
  stopped: true,
  first: true,
  inspector: false,
  persist: false,
  fontSize: 12,
  output: [],
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

class App extends React.Component<{}, AppState> {
  private readonly ref = React.createRef<HTMLDivElement>();
  private runtime?: SchemeRuntime;
  private readonly pending: string[] = [];
  private writing: boolean = false;

  constructor(props: {}) {
    super(props);
    this.state = { ...kDefaultState, ...loadSettings() };
    this.onClickOutside = this.onClickOutside.bind(this);
  }

  // useOnClickOutside(ref, () =>
  //   setState({ ...state, open: false, about: false })
  // );
  componentDidMount() {
    document.addEventListener("mousedown", this.onClickOutside);
  }

  componentWillUnmount() {
    document.removeEventListener("mousedown", this.onClickOutside);
  }

  onClickOutside(event: MouseEvent) {
    const target = event.target;
    if (
      !this.ref.current ||
      (target instanceof Element && this.ref.current.contains(target))
    ) {
      return;
    }
    this.setState({ open: false, about: false });
  }

  onWrite(str: string) {
    if (this.writing) {
      this.pending.push(str);
      return;
    }

    if (this.pending.length > 0) {
      this.pending.push(str);
      str = this.pending.join("");
      this.pending.splice(0, this.pending.length);
    }

    const parts = str.split("\n");
    if (!parts.length) {
      return;
    }
    const output = [...this.state.output];
    if (output.length == 0) {
      output.push(parts[0]);
    } else {
      output[output.length - 1] += parts[0];
    }
    parts.shift();
    output.push(...parts);
    this.writing = true;
    this.setState({ output: output }, () => {
      this.writing = false;
      if (this.pending.length) {
        window.requestAnimationFrame(() => this.onWrite(""));
      }
    });
  }

  async onInput(str: string): Promise<void> {
    this.onWrite("\n" + this.getPrompt() + str + "\n");
    try {
      if (!this.runtime) {
        this.runtime = await SchemeRuntime.load();
        this.runtime.addEventListener("write", (str) => this.onWrite(str));
        this.setState({ first: false, stopped: false }, () =>
          this.onWrite("\x1B[0;94mStarted runtime.\x1B[0m ")
        );
        return;
      } else if (!this.runtime || this.runtime.stopped) {
        await this.runtime.start();
        this.setState({ stopped: false }, () =>
          this.onWrite("\x1B[0;94mRestarted.\x1B[0m ")
        );
      }
      const result = this.runtime.processLine(str + "\n");
      if (this.runtime.stopped) {
        this.setState({ stopped: true });
      } else if (this.runtime.partial && this.state.prompt != kPartialPrompt) {
        this.setState({ prompt: kPartialPrompt });
      } else if (!this.runtime.partial && this.state.prompt != kRegularPrompt) {
        this.setState({ prompt: kRegularPrompt });
      }
      return; //result;
    } catch (err) {
      this.onWrite(`\x1B[0;31m${err}

\x1B[0;94mIt's unlikely that trying again will help ‾\\_(シ)_/‾ \x1B[0m
`);
    }
  }

  getEditorTheme() {
    if (this.state.editorTheme == "Same") {
      return false;
    } else if (this.state.editorTheme == "Dark") {
      return kSolarizedDark;
    } else {
      return kSolarizedLight;
    }
  }

  getPrompt() {
    return this.state.first
      ? "start:"
      : this.state.stopped
      ? "stopped:"
      : this.state.prompt;
  }

  render() {
    return (
      <ThemeProvider
        value={this.state.theme == "Dark" ? kSolarizedDark : kSolarizedLight}
      >
        <SchemeRuntimeProvider value={this.runtime}>
          <EditorThemeProvider value={this.getEditorTheme()}>
            <div
              style={{
                display: "grid",
                height: "calc(100vh - 5rem)",
                width: "calc(95vw - 7rem)",
                margin: "1rem auto 4rem",
                boxSizing: "border-box",
                boxShadow: "#444 0 0.5em 1em",
                overflowY: "scroll",
                fontSize: `${this.state.fontSize}pt`,
              }}
            >
              <Terminal
                prompt={this.getPrompt()}
                pause={this.state.stopped}
                welcomeMessage="Welcome to scheme.wasm"
                autofocus={!this.state.open && !this.state.inspector}
                fontSize={(4 * this.state.fontSize) / 3}
                onInput={(str) => this.onInput(str)}
                output={this.state.output}
              />
            </div>
            {this.state.inspector ? (
              <Flyout label="Inspector" fontSize={this.state.fontSize}>
                <HeapInspector scale={this.state.fontSize / 12} />
              </Flyout>
            ) : null}

            <div ref={this.ref}>
              <Burger
                open={this.state.open}
                onClick={() => this.setState({ open: !this.state.open })}
              />
              <SettingsMenu open={this.state.open}>
                <Settings
                  theme={this.state.theme}
                  editorTheme={this.state.editorTheme}
                  inspector={this.state.inspector}
                  fontSize={this.state.fontSize}
                  persist={this.state.persist}
                  onChange={(update) => {
                    if (update.persist) {
                      storeSettings(update);
                    } else if (this.state.persist) {
                      clearSettings();
                    }
                    this.setState({ ...update });
                  }}
                  onAbout={() => this.setState({ about: true, open: false })}
                />
              </SettingsMenu>
            </div>
            {this.state.about ? <About /> : null}
          </EditorThemeProvider>
        </SchemeRuntimeProvider>
      </ThemeProvider>
    );
  }
}

ReactDOM.render(<App />, document.getElementById("app"));
