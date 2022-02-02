import React from "react";
import { DataLine, TerminalData } from "./TerminalData";
import { TerminalInput } from "./TerminalInput";
import { ThemeContext } from "./ThemeProvider";

const TerminalEditor = React.lazy(async () => {
  await ((window as any).getMonaco as () => Promise<void>)();
  return import("./TerminalEditor");
});

interface TerminalProps {
  welcomeMessage?: React.ReactNode;
  output: DataLine[];
  prompt: string;
  waiting: boolean;
  pause: boolean;
  fontSize: number;
  onInput: (str: string) => Promise<void>;
}

interface TerminalState {
  history: string[];
  hidx: number;
  input: string;
  cached: string;
  editing: boolean;
  line: string;
  canPaste: boolean;
  initialVersion: number;
  currentVersion: number;
  highVersion: number;
}

const kDefaultState: TerminalState = {
  history: [],
  hidx: 0,
  input: "",
  cached: "",
  editing: false,
  line: "",
  canPaste: false,
  initialVersion: 0,
  currentVersion: 0,
  highVersion: 0,
};

export class Terminal extends React.Component<TerminalProps, TerminalState> {
  private readonly terminalRef = React.createRef<HTMLDivElement>();

  constructor(props: TerminalProps) {
    super(props);
    this.state = { ...kDefaultState };

    this.clipboardChange = this.clipboardChange.bind(this);
  }

  async onEnter(prompt: string, text: string) {
    const cmd = text;
    const history = [...this.state.history];

    // update history
    const hidx = history.findIndex((el) => el === cmd);
    if (hidx >= 0) {
      history.splice(hidx, 1);
    }
    history.push(cmd);

    await this.props.onInput(cmd);

    this.setState({
      input: "",
      history: history,
      hidx: history.length,
      cached: "",
    });
  }

  htmlify(raw: string) {
    return raw
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;");
  }

  onUp() {
    if (this.state.hidx <= 0) {
      return;
    }
    this.setState({
      hidx: this.state.hidx - 1,
      cached:
        this.state.hidx == this.state.history.length
          ? this.state.input
          : this.state.cached,
      input: this.htmlify(this.state.history[this.state.hidx - 1]),
    });
  }

  onDown() {
    if (this.state.hidx >= this.state.history.length) {
      return;
    } else if (this.state.hidx == this.state.history.length - 1) {
      this.setState({
        hidx: this.state.hidx + 1,
        input: this.state.cached,
        cached: "",
      });
    } else {
      this.setState({
        hidx: this.state.hidx + 1,
        input: this.htmlify(this.state.history[this.state.hidx + 1]),
      });
    }
  }

  onEscape(text: string) {
    this.setState({
      editing: true,
      line: text,
    });
  }

  async clipboardChange() {
    try {
      const content = await navigator.clipboard.readText();
      this.setState({ canPaste: !!content && content.length > 0 });
    } catch (err) {
      this.setState({ canPaste: false });
    }
  }

  componentDidUpdate(prevProps: TerminalProps, prevState: TerminalState) {
    if (this.state.editing != prevState.editing) {
      if (this.state.editing) {
        navigator.clipboard.addEventListener(
          "clipboardchange",
          this.clipboardChange
        );
        navigator.clipboard
          .readText()
          .then((content) =>
            this.setState({ canPaste: !!content && content.length > 0 })
          );
      } else {
        navigator.clipboard.removeEventListener(
          "clipboardchange",
          this.clipboardChange
        );
      }
    }
  }

  onDoneEditing(line: string) {
    this.setState({
      editing: false,
      line: "",
      input: this.htmlify(line),
    });
  }

  onContextMenu(evt: React.MouseEvent) {
    const sel = document.getSelection();
    if (!sel) {
      return;
    }
    if (sel.isCollapsed) {
      navigator.clipboard.readText().then((text) => {
        if (!text) {
          return;
        }
        this.setState({ input: this.state.input + text });
      });
    } else {
      const text = sel.toString();
      navigator.clipboard.writeText(text).then(() => sel.removeAllRanges());
    }
    evt.preventDefault();
  }

  render() {
    if (this.state.editing && !this.props.pause) {
      return (
        <React.Suspense fallback={<Loading />}>
          <TerminalEditor
            fontSize={this.props.fontSize}
            line={this.state.line}
            onDoneEditing={(line) => this.onDoneEditing(line)}
          />
        </React.Suspense>
      );
    }
    return (
      <ThemeContext.Consumer>
        {(theme) => (
          <div
            tabIndex={0}
            ref={this.terminalRef}
            style={{
              backgroundColor: theme.background,
              color: theme.foreground,
              fontFamily: "'Source Code Pro', monospace",
              padding: "0.5em",
              minWidth: "40rem",
              outline: "none",
            }}
            onContextMenu={(e) => this.onContextMenu(e)}
          >
            {this.props.welcomeMessage}
            <TerminalData text={this.props.output} />
            <TerminalInput
              prompt={this.props.prompt}
              color={theme.foreground}
              waiting={this.props.waiting}
              readonly={this.props.pause}
              value={this.state.input}
              onEnter={(text) => this.onEnter(this.props.prompt, text)}
              onUp={() => this.onUp()}
              onDown={() => this.onDown()}
              onEscape={(text) => this.onEscape(text)}
              parent={this.terminalRef.current || undefined}
            />
          </div>
        )}
      </ThemeContext.Consumer>
    );
  }
}

const Loading: React.FC = (props) => (
  <div
    style={{
      display: "flex",
      width: "100%",
      height: "100%",
      margin: 0,
      padding: 0,
      justifyContent: "space-around",
    }}
  >
    <svg style={{ alignSelf: "center" }} width="6em" viewBox="0 0 90 30">
      <circle cx="15" cy="15" r="10">
        <animate
          attributeName="r"
          values="10;0;10"
          dur="1s"
          repeatCount="indefinite"
        />
      </circle>
      <circle cx="45" cy="15" r="10">
        <animate
          attributeName="r"
          values="5;10;0;5"
          dur="1s"
          repeatCount="indefinite"
        />
      </circle>
      <circle cx="75" cy="15" r="10">
        <animate
          attributeName="r"
          values="0;10;0"
          dur="1s"
          repeatCount="indefinite"
        />
      </circle>
    </svg>
  </div>
);
