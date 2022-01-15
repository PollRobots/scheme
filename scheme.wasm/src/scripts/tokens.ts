export interface Token {
  type: string;
  content: string;
  line: number;
}

interface WhitespaceToken extends Token {
  type: "whitespace";
}

export function isWhitespaceToken(value: any): value is WhitespaceToken {
  return value?.type === "whitespace";
}

interface CommentToken extends Token {
  type: "comment";
}

export function isCommentToken(value: any): value is CommentToken {
  return value?.type === "comment";
}

interface DelimiterToken extends Token {
  type: "delimiter";
}

export function isDelimiterToken(value: any): value is DelimiterToken {
  return value?.type === "delimiter";
}

interface ElementToken extends Token {
  type: "element";
}

export function isElementToken(value: any): value is ElementToken {
  return value?.type === "element";
}

interface StringToken extends Token {
  type: "string";
}

export function isStringToken(value: any): value is StringToken {
  return value?.type === "string";
}

export interface MacroToken extends Token {
  type: "macro";
}

export function isMacroToken(value: any): value is MacroToken {
  return value?.type === "macro";
}

interface DefineToken extends Token {
  type: "define";
  content: "%define";
}

export function isDefineToken(value: any): value is DefineToken {
  return value?.type === "define" && value.content === "%define";
}

function isWhitespaceChar(char: number): boolean {
  return char == 0x20 || char == 0x09 || char == 0x0a || char == 0x0d;
}

function isDelimiterChars(char: string, next: string): boolean {
  return (
    char === "(" ||
    char === ")" ||
    (char === ";" && (next == ";" || next == ")")) ||
    (char === "%" && next === "(") ||
    (char === ")" && next === "%")
  );
}

function isHexDigit(char: string) {
  return char.length == 1 && !!char.match(/[a-fA-F0-9]/);
}

export function* tokenize(input: string): Generator<Token> {
  let state:
    | "unknown"
    | "whitespace"
    | "element"
    | "string"
    | "comment"
    | "multi-comment" = "unknown";
  let accum: string[] = [];
  let multiDepth = 0;
  let line = 0;

  const typeFromState = (state: string, contents: string) => {
    if (state !== "element") {
      return state;
    }
    if (contents === "%define") {
      return "define";
    } else if (contents.startsWith("%")) {
      return "macro";
    }
    return state;
  };

  const proceeds = (): Token => {
    const contents = accum.join("");
    const type = typeFromState(state, contents);
    accum = [];
    state = "unknown";
    return { type: type, content: contents, line: line };
  };

  for (let i = 0; i < input.length; i++) {
    const char = input[i];
    const next = i < input.length - 1 ? input[i + 1] : "";
    const curr = input.charCodeAt(i);

    if (char == "\n") {
      line++;
    }

    if (state === "unknown" || state === "whitespace") {
      if (isWhitespaceChar(curr)) {
        accum.push(char);
        state = "whitespace";
      } else if (isDelimiterChars(char, next)) {
        if (accum.length) {
          yield proceeds();
        }

        if (char == "(" && next == ";") {
          accum.push("(;");
          state = "multi-comment";
          multiDepth = 1;
          i++;
        } else if (char == ";" && next == ")") {
          yield { type: "delimiter", content: ";)", line: line };
          i++;
        } else if (char == "%" && next == "(") {
          yield { type: "delimiter", content: "%(", line: line };
          i++;
        } else if (char == ")" && next == "%") {
          yield { type: "delimiter", content: ")%", line: line };
          i++;
        } else if (char == "(") {
          yield { type: "delimiter", content: "(", line: line };
        } else if (char === ")") {
          yield { type: "delimiter", content: ")", line: line };
        } else if (char === ";" && next == ";") {
          accum.push(";;");
          state = "comment";
          i++;
        }
      } else if (char === '"') {
        if (accum.length) {
          yield proceeds();
        }
        accum.push(char);
        state = "string";
      } else {
        if (accum.length) {
          yield proceeds();
        }
        accum.push(char);
        state = "element";
      }
    } else if (state === "comment") {
      accum.push(char);
      if (curr == 0x0a) {
        yield proceeds();
      }
    } else if (state === "multi-comment") {
      accum.push(char);
      if (char == ";" && next == ")") {
        accum.push(next);
        i++;
        multiDepth--;
        if (multiDepth == 0) {
          state = "comment";
          yield proceeds();
        }
      } else if (char == "(" && next == ";") {
        accum.push(next);
        i++;
        multiDepth++;
      }
    } else if (state === "element") {
      if (
        isWhitespaceChar(curr) ||
        isDelimiterChars(char, next) ||
        char === '"'
      ) {
        yield proceeds();
        i--;
      } else {
        accum.push(char);
      }
    } else if (state === "string") {
      if (char === '"') {
        accum.push(char);
        yield proceeds();
      } else if (char === "\\" && (next === '"' || next === "\\")) {
        accum.push(char);
        accum.push(next);
        i++;
      } else {
        accum.push(char);
      }
    } else {
      console.error(`Unhandled state "${state}"`);
      return;
    }
  }

  if (accum.length) {
    yield proceeds();
  }
}
