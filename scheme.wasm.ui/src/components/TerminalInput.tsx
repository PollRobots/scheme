import React from "react";
import ContentEditable from "react-contenteditable";
import sanitizeHtml from "sanitize-html";

interface TerminalInputProps {
  value: string;
  prompt: string;
  onEnter: (text: string) => void;
  onUp: () => void;
  onDown: () => void;
  onEscape: (text: string) => void;
}

const kSanitizeConfig: sanitizeHtml.IOptions = {
  allowedTags: [],
};

export const TerminalInput: React.FunctionComponent<TerminalInputProps> = (
  props
) => {
  const text = React.useRef("");

  if (text.current !== props.value) {
    text.current = props.value;
  }

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
        html={sanitizeHtml(text.current, kSanitizeConfig)}
        onChange={(e) => (text.current = e.target.value)}
        onKeyDown={(e) => {
          if (e.key === "Enter") {
            props.onEnter(sanitizeHtml(text.current, kSanitizeConfig));
            text.current = "";
          } else if (e.key === "ArrowUp") {
            props.onUp();
          } else if (e.key === "ArrowDown") {
            props.onDown();
          } else if (e.key === "Escape" || (e.key === "e" && e.ctrlKey)) {
            props.onEscape(sanitizeHtml(text.current, kSanitizeConfig));
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
