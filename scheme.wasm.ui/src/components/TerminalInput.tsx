import React from "react";
import ContentEditable from "react-contenteditable";
import sanitizeHtml from "sanitize-html";
import { useOnClickOutside } from "./hooks";

interface TerminalInputProps {
  value: string;
  autofocus: boolean;
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

export const TerminalInput: React.FunctionComponent<TerminalInputProps> = (
  props
) => {
  const [state, setState] = React.useState({ text: props.value });
  const ref = React.useRef<HTMLElement>(null);
  // const text = React.useRef("");

  React.useEffect(() => {
    if (!props.autofocus) {
      return;
    }
    let t = 0;
    const timeoutFn = (): number => {
      if (
        props.autofocus &&
        ref.current &&
        ref.current !== document.activeElement
      ) {
        ref.current.focus();
      }
      return window.setTimeout(() => {
        t = timeoutFn();
      }, 250);
    };
    t = timeoutFn();
    return () => {
      clearTimeout(t);
    };
  }, [props.autofocus]);

  React.useEffect(() => {
    if (ref.current) {
      ref.current.innerHTML = props.value;
    }
  }, [props.value]);

  const getValue = () => {
    return sanitizeHtml(
      ref.current ? ref.current.innerHTML : state.text,
      kSanitizeConfig
    );
  };

  return (
    <div style={{ display: "flex" }}>
      <span style={{ whiteSpace: "pre-wrap" }}>{props.prompt}</span>
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
        innerRef={ref}
        html={sanitizeHtml(state.text, kSanitizeConfig)}
        spellCheck={false}
        onChange={(e) => setState({ text: e.target.value })}
        onKeyDown={(e) => {
          if (e.key === "Enter") {
            props.onEnter(getValue());
            setState({ text: "" });
          } else if (e.key === "ArrowUp") {
            props.onUp();
          } else if (e.key === "ArrowDown") {
            props.onDown();
          } else if (e.key === "Escape" || (e.key === "e" && e.ctrlKey)) {
            props.onEscape(getValue());
          } else {
            return;
          }
          e.stopPropagation();
          e.preventDefault();
        }}
      />
    </div>
  );
};
