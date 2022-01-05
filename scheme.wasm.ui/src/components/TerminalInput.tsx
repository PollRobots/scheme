import React from "react";
import ContentEditable from "react-contenteditable";
import sanitizeHtml from "sanitize-html";
import Prism from "prismjs";
import "prismjs/components/prism-scheme";
import "prismjs/themes/prism-solarizedlight.css";

interface TerminalInputProps {
  value: string;
  readonly: boolean;
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
          this.props.value,
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
    if (e.key === "Enter") {
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
    return (
      <div style={{ display: "flex" }}>
        <span style={{ whiteSpace: "pre-wrap" }}>{this.props.prompt}</span>
        <ContentEditable
          style={{
            display: "inline-block",
            whiteSpace: "pre-wrap",
            background: "inherit",
            border: "none",
            width: `calc(100% - ${prompt.length}em)`,
            font: "inherit",
            fontSize: "inherit",
            color: "inherit",
            padding: 0,
            margin: 0,
            outline: "none",
            caretColor: "none",
            wordWrap: "break-word",
            wordBreak: "break-all",
          }}
          title="Open editor with Ctrl+E, or Escape"
          innerRef={this.ref}
          html={Prism.highlight(
            this.state.text,
            Prism.languages.scheme,
            "scheme"
          )}
          spellCheck={false}
          onChange={(e) =>
            this.setState({ text: this.cleanHtml(e.target.value) })
          }
          onKeyDown={(e) => this.onKeyDown(e)}
        />
      </div>
    );
  }
}
