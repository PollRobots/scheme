import React from "react";
import ContentEditable from "react-contenteditable";
import sanitizeHtml from "sanitize-html";
import Prism from "prismjs";
import "prismjs/components/prism-scheme";
import "prismjs/themes/prism-solarizedlight.css";
import { ThemeContext } from "./ThemeProvider";
import animations from "../styles/animations.module.css";
import { reference } from "../util";

reference(animations);

interface TerminalInputProps {
  value: string;
  readonly: boolean;
  waiting: boolean;
  prompt: string;
  onEnter: (text: string) => void;
  onUp: () => void;
  onDown: () => void;
  onEscape: (text: string) => void;
  parent?: HTMLElement;
}

interface TerminalInputState {
  text: string;
}

const kSanitizeConfig: sanitizeHtml.IOptions = {
  allowedTags: [],
};

export class TerminalInput extends React.Component<
  TerminalInputProps,
  TerminalInputState
> {
  private readonly ref = React.createRef<HTMLElement>();

  constructor(props: TerminalInputProps) {
    super(props);
    this.state = { text: props.value };
    this.onParentKeyPress = this.onParentKeyPress.bind(this);
  }

  onParentKeyPress(evt: KeyboardEvent) {
    if (!this.ref.current || !evt.target) {
      return;
    }
    if (
      evt.target instanceof HTMLElement &&
      evt.target.contains(this.ref.current)
    ) {
      this.ref.current.focus();
      this.ref.current.scrollIntoView(false);
    }
  }

  componentDidMount() {
    if (this.ref.current) {
      this.ref.current.scrollIntoView(false);
      this.ref.current.focus();

      const range = document.createRange();
      range.selectNodeContents(this.ref.current);
      range.collapse(false);
      const selection = window.getSelection();
      selection?.removeAllRanges();
      selection?.addRange(range);
    }
  }

  componentWillUnmount() {
    if (this.props.parent) {
      this.props.parent.removeEventListener("keypress", this.onParentKeyPress);
    }
  }

  componentDidUpdate(prevProps: TerminalInputProps) {
    if (prevProps.value != this.props.value) {
      if (this.ref.current) {
        this.ref.current.innerHTML = Prism.highlight(
          this.cleanHtml(this.props.value),
          Prism.languages.scheme,
          "scheme"
        );
        Prism.highlightElement(this.ref.current);
      }
    }
    if (prevProps.parent !== this.props.parent) {
      if (prevProps.parent) {
        prevProps.parent.removeEventListener("keypress", this.onParentKeyPress);
      }
      if (this.props.parent) {
        this.props.parent.addEventListener("keypress", this.onParentKeyPress);
      }
    }
  }

  cleanHtml(dirty: string) {
    return sanitizeHtml(dirty, kSanitizeConfig)
      .replaceAll("&gt;", ">")
      .replaceAll("&lt;", "<")
      .replaceAll("&amp;", "&");
  }

  getValue() {
    return this.cleanHtml(
      this.ref.current ? this.ref.current.innerHTML : this.state.text
    );
  }

  onKeyDown(e: React.KeyboardEvent) {
    if (e.key === "Enter" && !this.props.waiting) {
      this.props.onEnter(this.getValue());
      this.setState({ text: "" });
    } else if (this.props.readonly) {
      this.setState({ text: "" });
      // ignore everything while readonly
    } else if (e.key === "ArrowUp") {
      this.props.onUp();
    } else if (e.key === "ArrowDown") {
      this.props.onDown();
    } else if (e.key === "Escape" || (e.key === "e" && e.ctrlKey)) {
      this.props.onEscape(this.getValue());
    } else {
      return;
    }
    e.stopPropagation();
    e.preventDefault();
  }

  render() {
    const spinner = this.props.waiting ? <Spinner /> : null;
    const prompt = (
      <span
        style={{ whiteSpace: "pre-wrap", gridColumnStart: 1, gridRowStart: 1 }}
      >
        {this.props.waiting
          ? "".padEnd(this.props.prompt.length, " ")
          : this.props.prompt}
      </span>
    );
    return (
      <div
        style={{
          display: "grid",
          gridTemplateColumns: "auto 1fr",
          position: "relative",
        }}
      >
        {spinner}
        {prompt}
        <ContentEditable
          style={{
            display: "inline-block",
            whiteSpace: "pre-wrap",
            background: "inherit",
            border: "none",
            font: "inherit",
            fontSize: "inherit",
            color: "inherit",
            padding: 0,
            margin: 0,
            outline: "none",
            wordWrap: "break-word",
            wordBreak: "break-all",
            gridColumnStart: 2,
            gridRowStart: 1,
          }}
          title="Open editor with Ctrl+E, or Escape"
          innerRef={this.ref}
          html={this.state.text}
          spellCheck={false}
          onChange={(e) => this.setState({ text: e.target.value })}
          onKeyDown={(e) => this.onKeyDown(e)}
        />
        <div
          style={{
            display: "inline-block",
            whiteSpace: "pre-wrap",
            background: "inherit",
            border: "none",
            font: "inherit",
            fontSize: "inherit",
            color: "inherit",
            padding: 0,
            margin: 0,
            outline: "none",
            userSelect: "none",
            caretColor: "none",
            wordWrap: "break-word",
            wordBreak: "break-all",
            gridColumnStart: 2,
            gridRowStart: 1,
          }}
          dangerouslySetInnerHTML={{
            __html: Prism.highlight(
              this.cleanHtml(this.state.text),
              Prism.languages.scheme,
              "scheme"
            ),
          }}
        />
      </div>
    );
  }
}

const Spinner: React.FunctionComponent = (props) => {
  const theme = React.useContext(ThemeContext);

  return (
    <div
      style={{
        width: "1em",
        height: "1em",
        margin: 0,
        padding: 0,
        boxSizing: "border-box",
        position: "relative",
        overflow: "clip",
        gridColumnStart: 1,
        gridRowStart: 1,
      }}
    >
      <div
        style={{
          zIndex: 10,
          background: `linear-gradient(${theme.foreground}, ${theme.background})`,
          width: "1em",
          height: "1em",
          borderRadius: "0.5em",
          margin: 0,
          padding: 0,
          boxSizing: "border-box",
          position: "absolute",
          top: 0,
          left: 0,
          animationDuration: "1s",
          animationIterationCount: "infinite",
          animationName: "clockwise-spin",
          animationTimingFunction: "linear",
        }}
      >
        <div
          style={{
            background: theme.background,
            width: "0.6em",
            height: "0.6em",
            position: "absolute",
            left: "0.2em",
            top: "0.2em",
            borderRadius: "0.3em",
          }}
        />
        <div
          style={{
            background: theme.background,
            width: "1em",
            height: "1em",
            position: "absolute",
            left: "0.5em",
            top: "0.5em",
          }}
        />
      </div>
    </div>
  );
};
