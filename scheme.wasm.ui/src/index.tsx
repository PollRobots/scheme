import React from "react";
import ReactDOM from "react-dom";
import UAParser from "ua-parser-js";
import { About } from "./components/About";
import { Burger } from "./components/Burger";
import { Flyout } from "./components/Flyout";
import { HeapInspector } from "./components/HeapInspector";
import { SchemeRuntimeProvider } from "./components/SchemeRuntimeProvider";
import { Settings, SettingsBase } from "./components/Settings";
import { SettingsMenu } from "./components/SettingsMenu";
import { Terminal } from "./components/Terminal";
import { EditorThemeProvider, ThemeProvider } from "./components/ThemeProvider";
import { kSolarizedDark, kSolarizedLight } from "./monaco/solarized";
import { reference } from "./util";

import fonts from "./styles/fonts.module.css";
import css from "./styles/page.module.css";
import { DataLine } from "./components/TerminalData";
import { RuntimeWorker } from "./RuntimeWorker";

reference(fonts, css);

interface AppState {
  theme: string;
  editorTheme: string;
  prompt: string;
  fontSize: number;
  open: boolean;
  stopped: boolean;
  waiting: boolean;
  about: boolean;
  first: boolean;
  inspector: boolean;
  persist: boolean;
  output: DataLine[];
  clock: number;
  isMobile: boolean;
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
  waiting: false,
  first: true,
  inspector: false,
  persist: false,
  fontSize: 12,
  output: [],
  clock: 0,
  isMobile: false,
};

const kDefaultRootStyle: React.CSSProperties = {
  display: "grid",
  height: "calc(100vh - 5rem)",
  width: "calc(95vw - 7rem)",
  margin: "1rem auto 4rem",
  boxSizing: "border-box",
  boxShadow: "#444 0 0.5em 1em",
  overflowY: "scroll",
};

const kMobileRootStyle: React.CSSProperties = {
  display: "grid",
  height: "100vh",
  width: "100vw",
  margin: "0",
  boxSizing: "border-box",
  overflowY: "scroll",
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
  private runtime?: RuntimeWorker;
  private readonly pending: DataLine[] = [];
  private writing: boolean = false;

  constructor(props: {}) {
    super(props);
    const uaParser = new UAParser();
    const device = uaParser.getDevice();

    this.state = {
      ...kDefaultState,
      ...loadSettings(),
      isMobile: device.type === "mobile",
    };
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

  onWrite(
    mode: "raw" | "datum" | "prompted",
    str: string,
    prompt?: string,
    first?: boolean
  ) {
    if (this.writing) {
      this.pending.push({
        type: mode,
        text: str,
        prompt: prompt,
        first: first,
      });
      return;
    }

    if (this.pending.length == 0 && str.length == 0 && mode !== "prompted") {
      return;
    }

    this.pending.push({ type: mode, text: str, prompt: prompt, first: first });
    let curr: DataLine;
    const parts: DataLine[] = [];
    this.pending.forEach((el) => {
      if (
        !curr ||
        curr.type != el.type ||
        curr.prompt != el.prompt ||
        curr.first != el.first
      ) {
        curr = el;
        parts.push(curr);
      } else {
        curr.text += el.text;
      }
      if (curr.text.indexOf("\n") >= 0) {
        const lines = curr.text.split("\n");
        curr.text = lines[0];
        lines.shift();
        lines.forEach((line) => {
          curr = {
            ...curr,
            text: line,
            first: curr.first === undefined ? undefined : false,
          };
          parts.push(curr);
        });
      }
    });
    this.pending.splice(0, this.pending.length);

    if (!parts.length) {
      return;
    }
    const output = [...this.state.output];
    if (output.length == 0) {
      output.push(parts[0]);
    } else if (
      output[output.length - 1].type === parts[0].type &&
      output[output.length - 1].prompt === parts[0].prompt &&
      output[output.length - 1].first === parts[0].first
    ) {
      output[output.length - 1].text += parts[0].text;
    } else {
      output.push(parts[0]);
    }
    parts.shift();
    parts.forEach((el) => output.push(el));
    this.writing = true;
    this.setState(
      {
        output: output,
        clock:
          output.length !== this.state.output.length
            ? this.state.clock + 1
            : this.state.clock,
      },
      () => {
        this.writing = false;
        if (this.pending.length) {
          window.requestAnimationFrame(() => this.onWrite("raw", ""));
        }
      }
    );
  }

  async onInput(str: string): Promise<void> {
    this.onWrite("prompted", str, this.getPrompt(), true);
    try {
      if (!this.runtime) {
        this.runtime = new RuntimeWorker();
        await this.runtime.load();
        this.runtime.addEventListener("write", (str) =>
          this.onWrite("raw", str)
        );
        this.runtime.addEventListener("status", () => {
          if (!this.runtime) {
            return;
          }
          this.setState({
            stopped: this.runtime.stopped,
            waiting: this.runtime.waiting,
            prompt: this.runtime.partial ? kPartialPrompt : kRegularPrompt,
          });
        });
        this.setState({ first: false });
        await this.runtime.start();
      } else if (!this.runtime || this.runtime.stopped) {
        await this.runtime.start();
      } else if (this.runtime.waiting) {
        return;
      }
      this.runtime.processLine(str + "\n");
      this.setState({ clock: this.state.clock + 1 });
    } catch (err) {
      this.onWrite(
        "raw",
        `\x1B[0;31m${err}

\x1B[0;94mIt's unlikely that trying again will help ‾\\_(シ)_/‾ \x1B[0m
`
      );
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
    const rootStyle = this.state.isMobile
      ? kMobileRootStyle
      : kDefaultRootStyle;
    return (
      <ThemeProvider
        value={this.state.theme == "Dark" ? kSolarizedDark : kSolarizedLight}
      >
        <SchemeRuntimeProvider value={this.runtime}>
          <EditorThemeProvider value={this.getEditorTheme()}>
            <div style={{ ...rootStyle, fontSize: `${this.state.fontSize}pt` }}>
              <Terminal
                prompt={this.getPrompt()}
                pause={this.state.stopped}
                waiting={this.state.waiting}
                welcomeMessage="Welcome to scheme.wasm"
                fontSize={(4 * this.state.fontSize) / 3}
                onInput={(str) => this.onInput(str)}
                output={this.state.output}
              />
            </div>
            {this.state.inspector ? (
              <Flyout label="Inspector" fontSize={this.state.fontSize}>
                <HeapInspector
                  scale={this.state.fontSize / 12}
                  clock={this.state.clock}
                />
              </Flyout>
            ) : null}

            <div ref={this.ref}>
              <Burger
                open={this.state.open}
                visibleBack={this.state.isMobile}
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

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById("app")
);
