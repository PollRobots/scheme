import * as fluent from "./fluent";

interface Token {
  type: string;
  content: string;
}

interface WhitespaceToken extends Token {
  type: "whitespace";
}

function isWhitespaceToken(value: any): value is WhitespaceToken {
  return value?.type === "whitespace";
}

interface CommentToken extends Token {
  type: "comment";
}

function isCommentToken(value: any): value is CommentToken {
  return value?.type === "comment";
}

interface DelimiterToken extends Token {
  type: "delimiter";
}

function isDelimiterToken(value: any): value is DelimiterToken {
  return value?.type === "delimiter";
}

interface ElementToken extends Token {
  type: "element";
}

function isElementToken(value: any): value is ElementToken {
  return value?.type === "element";
}

interface StringToken extends Token {
  type: "string";
}

function isStringToken(value: any): value is StringToken {
  return value?.type === "string";
}

interface MacroToken extends Token {
  type: "macro";
}

function isMacroToken(value: any): value is MacroToken {
  return value?.type === "macro";
}

interface DefineToken extends Token {
  type: "define";
  content: "%define";
}

function isDefineToken(value: any): value is DefineToken {
  return value?.type === "define" && value.content === "%define";
}

function isWhitespaceChar(char: number): boolean {
  return char == 0x20 || char == 0x09 || char == 0x0a || char == 0x0d;
}

function isDelimiterChars(char: string, next: string): boolean {
  return (
    char === "(" ||
    char === ")" ||
    (char === ";" && (next == ";" || next == ")"))
  );
}

function isHexDigit(char: string) {
  return char.length == 1 && !!char.match(/[a-fA-F0-9]/);
}

export function* tokenize(input: string): Generator<Token> {
  let state: "unknown" | "whitespace" | "element" | "string" | "comment" | "multi-comment"=
    "unknown";
  let accum: string[] = [];
  let multiDepth = 0;

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
    return { type: type, content: contents };
  };

  for (let i = 0; i < input.length; i++) {
    const char = input[i];
    const next = i < input.length - 1 ? input[i + 1] : "";
    const curr = input.charCodeAt(i);

    if (state === "unknown" || state === "whitespace") {
      if (isWhitespaceChar(curr)) {
        accum.push(char);
        state = "whitespace";
      } else if (isDelimiterChars(char, next)) {
        if (accum.length) {
          yield proceeds();
        }

        if (char == "(" && next == ";") {
          accum.push('(;')
          state = "multi-comment";
          multiDepth = 1;
          i++;
        } else if (char == ";" && next == ")") {
          yield { type: "delimiter", content: ";)" };
          i++;
        } else if (char == "(") {
          yield { type: "delimiter", content: "(" };
        } else if (char === ")") {
          yield { type: "delimiter", content: ")" };
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
    } else if(state === 'multi-comment') {
      accum.push(char);
      if (char == ';' && next == ')') {
        accum.push(next);
        i++;
        multiDepth--;
        if (multiDepth == 0) {
          state = "comment";
          yield proceeds();
        }
      } else if (char == '(' && next == ';') {
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

abstract class ParsedWat {
  abstract get type(): "list" | "atom";
  abstract toString(): string;
  abstract expand(scope: ExpansionScope): ParsedWat | ParsedWat[];
}

interface MacroDefinition {
  name: string;
  args: string[];
  body: ParsedWat[];
}

class ExpansionScope {
  private parent_: ExpansionScope | undefined;
  private readonly definitions_: Map<string, MacroDefinition> = new Map();

  constructor(parent?: ExpansionScope) {
    this.parent_ = parent;
  }

  add(macroDefinition: MacroDefinition) {
    this.definitions_.set(macroDefinition.name, macroDefinition);
  }

  lookup(name: string): MacroDefinition | undefined {
    if (this.definitions_.has(name)) {
      return this.definitions_.get(name);
    }

    return this.parent_?.lookup(name);
  }
}

class List extends ParsedWat {
  private readonly elements_: ParsedWat[];
  private readonly isComment_: boolean;

  constructor(elements: ParsedWat[], isComment?: boolean) {
    super();
    this.elements_ = elements;
    this.isComment_ = !!isComment;
  }

  get type(): "list" | "atom" {
    return "list";
  }

  get elements(): ParsedWat[] {
    return this.elements_;
  }

  get isComment(): boolean {
    return this.isComment_;
  }

  get isDefinition(): boolean {
    const atom = this.firstSignificantAtom();
    return atom !== undefined && isDefineToken(atom.token);
  }

  get isMacro(): boolean {
    const atom = this.firstSignificantAtom();
    return atom !== undefined && isMacroToken(atom.token);
  }

  get macroName(): string {
    const atom = this.firstSignificantAtom();
    if (atom !== undefined && isMacroToken(atom.token)) {
      return atom.token.content;
    }
    throw new Error("This is not a macro");
  }

  // a macro definition is a list, with the following format
  // ( %define %name (%arg1 %arg2 ...) ...)
  // for example:
  //    (%define %inc (%var)
  //      (local.set %var (i32.add (local.get %var) (i32.const 1)))
  //    )
  // usage:
  //    (%inc $index)
  // expands to
  //    (local.set $index (i32.add (local.get $index) (i32.const 1)))
  buildDefinition(): MacroDefinition {
    const macroDefinition = Array.from(
      fluent.take(this.getSignificantElements(), 3)
    );
    // The length must be 3, to include a define token, a macro token and a
    // list of argument names (each of which must be a macro token).
    if (
      macroDefinition.length != 3 ||
      !isAtom(macroDefinition[0]) ||
      !isDefineToken(macroDefinition[0].token) ||
      !isAtom(macroDefinition[1]) ||
      !isMacroToken(macroDefinition[1].token) ||
      !isList(macroDefinition[2]) ||
      !macroDefinition[2].isArgList()
    ) {
      throw new Error("Poorly defined macro");
    }

    const name = macroDefinition[1].token.content;
    const argList = macroDefinition[2].argList.map((el) => el.content);

    const bodyIndex = 1 + this.elements.indexOf(macroDefinition[2]);

    const body = this.elements.slice(bodyIndex);

    return { name: name, args: argList, body: body };
  }

  expandMacro(macroDefinition: MacroDefinition): ParsedWat[] {
    const args = Array.from(fluent.skip(this.getSignificantElements(), 1));
    if (args.length != macroDefinition.args.length) {
      throw new Error(
        `Macro ${this.macroName} called with ${args.length} args, expecting ${macroDefinition.args.length}`
      );
    }
    const argMap: Map<string, ParsedWat> = new Map();
    for (let i = 0; i != args.length; i++) {
      argMap.set(macroDefinition.args[i], args[i]);
    }

    return macroDefinition.body.map((el) => List.expandMacroItem(el, argMap));
  }

  private static expandMacroItem(
    item: ParsedWat,
    argMap: Map<string, ParsedWat>
  ): ParsedWat {
    if (isAtom(item)) {
      const token = item.token;
      if (isMacroToken(token)) {
        if (argMap.has(token.content)) {
          return argMap.get(token.content) as ParsedWat;
        }
      }
      return item;
    }

    const list = item as List;
    if (list.isComment) {
      return list;
    }
    const expanded = list.elements.map((el) =>
      List.expandMacroItem(el, argMap)
    );
    return new List(expanded);
  }

  private isArgList(): boolean {
    return (
      fluent.some(
        this.getSignificantElements(),
        (el) => isAtom(el) && isMacroToken(el.token)
      ) || fluent.empty(this.getSignificantElements())
    );
  }

  private get argList(): MacroToken[] {
    return Array.from(
      fluent.map(this.getSignificantElements(), (el) => {
        if (isAtom(el)) {
          const token = el.token;
          if (isMacroToken(token)) {
            return token;
          }
        }
        throw new Error(
          `Unexpected element in argument list '${el.toString()}'`
        );
      })
    );
  }

  private firstSignificantAtom(): Atom | undefined {
    const iter = this.getSignificantElements();
    const first = iter.next();
    if (!first.done && isAtom(first.value)) {
      return first.value;
    }
  }

  private *getSignificantElements(): Generator<ParsedWat> {
    for (const element of this.elements) {
      if (!isAtom(element)) {
        yield element;
        continue;
      }

      const token = element.token;

      if (!isWhitespaceToken(token) && !isCommentToken(token)) {
        yield element;
      }
    }
  }

  toString(): string {
    if (this.isComment) {
      return `(;${this.elements.map((el) => el.toString()).join("")};)`;
    } else {
      return `(${this.elements.map((el) => el.toString()).join("")})`;
    }
  }

  expand(scope: ExpansionScope): ParsedWat | ParsedWat[] {
    if (this.isComment) {
      return this;
    }

    if (this.isDefinition) {
      const defn = this.buildDefinition();
      scope.add(defn);
      return new List([this], true);
    } else if (this.isMacro) {
      const name = this.macroName;
      const defn = scope.lookup(name);
      if (defn) {
        return this.expandMacro(defn)
          .map((el) => {
            if (isList(el)) {
              const twoex = el.expand(scope);
              if (Array.isArray(twoex)) {
                return twoex;
              } else {
                return [twoex];
              }
            } else {
              return [el];
            }
          })
          .flat();
      }
    }

    const currScope = new ExpansionScope(scope);

    const expandedElements: ParsedWat[] = [];
    for (const el of this.elements) {
      const expanded = el.expand(currScope);
      if (Array.isArray(expanded)) {
        expandedElements.push(...expanded);
      } else {
        expandedElements.push(expanded);
      }
    }

    return new List(expandedElements);
  }
}

function isList(value: any): value is List {
  return value && value.type === "list";
}

class Atom extends ParsedWat {
  private readonly token_: Token;

  constructor(token: Token) {
    super();
    this.token_ = token;
  }

  get type(): "list" | "atom" {
    return "atom";
  }

  get token(): Token {
    return this.token_;
  }

  toString(): string {
    return this.token.content;
  }

  expand(scope: ExpansionScope): ParsedWat | ParsedWat[] {
    return this;
  }
}

function isAtom(value: any): value is Atom {
  return value && value.type === "atom";
}

export function parse(input: string): ParsedWat[] {
  const parsed: ParsedWat[] = [];

  const listStack: ParsedWat[][] = [];
  const typeStack: string[] = [];
  let curr = parsed;
  let currType = "";

  for (const token of tokenize(input)) {
    if (!isDelimiterToken(token)) {
      curr.push(new Atom(token));
      continue;
    }
    if (token.content === "(") {
      listStack.push(curr);
      typeStack.push(currType);
      curr = [];
      currType = "(";
    } else if (token.content === ")") {
      if (currType === "(") {
        const list = new List(curr);
        curr = listStack.pop() || [];
        currType = typeStack.pop() || "";
        curr.push(list);
      } else {
        throw new Error(`Unmatched (, got ${JSON.stringify(token.content)}`);
      }
    } else if (token.content === "(;") {
      listStack.push(curr);
      typeStack.push(currType);
      curr = [];
      currType = "(";
    } else if (token.content === ";)") {
      if (currType === "(;") {
        const list = new List(curr, true);
        curr = listStack.pop() || [];
        currType = typeStack.pop() || "";
        curr.push(list);
      } else {
        throw new Error(
          `Unmatched '${currType}', got ${JSON.stringify(token.content)}`
        );
      }
    } else {
      throw new Error(`Unexpected delimiter ${JSON.stringify(token.content)}`);
    }
  }

  if (listStack.length) {
    throw new Error("Unbalanced paren");
  }

  return parsed;
}

export function emit(parsed: ParsedWat[]) {
  const scope = new ExpansionScope();

  const output: string[] = [];

  for (const element of parsed) {
    const expanded = element.expand(scope);
    if (Array.isArray(expanded)) {
      expanded.forEach((el) => {
        output.push(el.toString());
      });
    } else {
      output.push(expanded.toString());
    }
  }

  return output;
}
