import React from "react";
import ContentEditable from "react-contenteditable";
import { render } from "react-dom";
import sanitizeHtml from "sanitize-html";
import { useOnClickOutside } from "./hooks";

interface TerminalInputProps {
  value: string;
  autofocus: boolean;
  readonly: boolean;
  prompt: string;
  onEnter: (text: string) => void;
  onUp: () => void;
  onDown: () => void;
  onEscape: (text: string) => void;
  inputRef?: React.RefObject<HTMLElement>;
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
  }

  componentDidUpdate(prevProps: TerminalInputProps) {
    if (prevProps.autofocus != this.props.autofocus) {
      const timeoutFn = () => {
        if (
          this.props.autofocus &&
          this.ref.current &&
          this.ref.current !== document.activeElement
        ) {
          this.ref.current.focus();
        }
        if (this.props.autofocus) {
          return window.setTimeout(() => {
            timeoutFn();
          }, 250);
        }
      };
    }
    if (prevProps.value != this.props.value) {
      if (this.ref.current) {
        this.ref.current.innerHTML = this.props.value;
      }
    }
  }

  getValue() {
    return sanitizeHtml(
      this.ref.current ? this.ref.current.innerHTML : this.state.text,
      kSanitizeConfig
    )
      .replace("&gt;", ">")
      .replace("&lt;", "<")
      .replace("&amp;", "&");
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
          innerRef={this.ref}
          html={sanitizeHtml(this.state.text, kSanitizeConfig)}
          spellCheck={false}
          onChange={(e) => this.setState({ text: e.target.value })}
          onKeyDown={(e) => this.onKeyDown(e)}
        />
      </div>
    );
  }
}
