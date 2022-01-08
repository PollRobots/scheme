import React from "react";
import Prism from "prismjs";

export interface DataLine {
  type: "raw" | "datum" | "prompted";
  text: string;
  prompt?: string;
  first?: boolean;
}

interface TerminalDataProps {
  text: DataLine[];
}

export const TerminalData: React.FunctionComponent<TerminalDataProps> = (
  props
) => (
  <div>
    {props.text.map((el, i) => {
      return <TerminalDataLine key={`line${i}`} {...el} />;
    })}
  </div>
);

const TerminalDataLine: React.FunctionComponent<DataLine> = (props) => {
  if (!props.text.length && props.type !== "prompted") {
    return (
      <div>
        <span
          style={{
            whiteSpace: "pre-wrap",
            wordBreak: "break-all",
            userSelect: "none",
          }}
        >
          {" "}
        </span>
      </div>
    );
  }

  if (props.type === "prompted") {
    const prompt = props.prompt || "";
    return (
      <div>
        <span
          style={{
            whiteSpace: "pre-wrap",
            wordBreak: "break-all",
            userSelect: "none",
          }}
        >
          {props.first ? prompt : "".padEnd(prompt?.length, " ")}
        </span>
        <span
          style={{ whiteSpace: "pre-wrap", wordBreak: "break-all" }}
          dangerouslySetInnerHTML={{
            __html: Prism.highlight(
              props.text,
              Prism.languages.scheme,
              "scheme"
            ),
          }}
        />
      </div>
    );
  }

  return (
    <div>
      <span style={{ whiteSpace: "pre-wrap", wordBreak: "break-all" }}>
        {AnsiEscaper(props.text)}
      </span>
    </div>
  );
};

const kColor: Record<number, string> = {
  30: "#073642", // 'black',
  31: "#dc322f", // 'red',
  32: "#859900", // 'green',
  33: "#b58900", // 'yellow',
  34: "#268bd2", // 'blue',
  35: "#d33682", // 'magenta',
  36: "#2aa198", // 'cyan',
  37: "#eee8d5", // 'white'
  40: "#073642", // 'black',
  41: "#dc322f", // 'red',
  42: "#859900", // 'green',
  43: "#b58900", // 'yellow',
  44: "#268bd2", // 'blue',
  45: "#d33682", // 'magenta',
  46: "#2aa198", // 'cyan',
  47: "#eee8d5", // 'white'
  90: "#002b36", // 'brblack',
  91: "#cb4b16", // 'brred',
  92: "#586e75", // 'brgreen',
  93: "#657b83", // 'bryellow',
  94: "#839496", // 'brblue',
  95: "#6c71c4", // 'brmagenta',
  96: "#93a1a1", // 'brcyan',
  97: "#fdf6e3", // 'brwhite'
  100: "#002b36", // 'brblack',
  101: "#cb4b16", // 'brred',
  102: "#586e75", // 'brgreen',
  103: "#657b83", // 'bryellow',
  104: "#839496", // 'brblue',
  105: "#6c71c4", // 'brmagenta',
  106: "#93a1a1", // 'brcyan',
  107: "#fdf6e3", // 'brwhite'
};

function AnsiEscaper(str: string): React.ReactNode {
  const nodes: React.ReactNode[] = [];
  const chars = Array.from(str);
  let start = 0;
  let escapeCount = 0;
  let foreground: string | undefined;
  let background: string | undefined;

  for (let index = 0; index < chars.length; index++) {
    const cp = chars[index].codePointAt(0);
    if (cp !== 0x1b) {
      continue;
    }
    escapeCount++;

    // this is an escape sequence
    // add string so far (if any)
    if (index > start) {
      const partial = chars.slice(start, index).join("");
      nodes.push(
        <span style={{ color: foreground, background: background }} key={index}>
          {partial}
        </span>
      );
    }
    index++;
    if (index == chars.length) {
      // orphan escape at the end of the string, simply break
      break;
    }

    const e1 = chars[index].codePointAt(0) || 0;
    if (e1 == 0x5b) {
      // this is a CSI
      // accumulate all chars up to terminating
      index++;
      const cmdBuf: string[] = [];
      let final = "";
      while (index < chars.length) {
        const e2 = chars[index].codePointAt(0) || 0;
        if (e2 >= 0x40 && e2 <= 0x7e) {
          final = chars[index];
          break;
        }
        cmdBuf.push(chars[index]);
        index++;
      }
      const cmd = cmdBuf.join("");
      if (final == "m") {
        const parts = cmd.split(";").map((el) => {
          const num = Number(el);
          return isNaN(num) ? 0 : num;
        });
        for (let pidx = 0; pidx < parts.length; pidx++) {
          const part = parts[pidx];
          if (part == 0) {
            foreground = undefined;
            background = undefined;
          } else if ((part >= 30 && part <= 37) || (part >= 90 && part <= 97)) {
            foreground = kColor[part];
          } else if (
            (part >= 40 && part <= 47) ||
            (part >= 100 && part <= 107)
          ) {
            background = kColor[part];
          } else {
            break;
          }
        }
      }
    } else if (e1 >= 0x40 && e1 <= 0x5f) {
      // This is an Fe sequence, which we don't support, simply skip
    }
    start = index + 1;
  }

  if (start > 0 && start < chars.length) {
    const partial = chars.slice(start).join("");
    nodes.push(
      <span
        style={{ color: foreground, background: background }}
        key={chars.length}
      >
        {partial}
      </span>
    );
  }

  if (escapeCount == 0) {
    return str;
  } else {
    return nodes;
  }
}
