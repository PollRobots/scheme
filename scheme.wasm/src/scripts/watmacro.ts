import * as fluent from "./fluent";
import child_process from "child_process";

interface Token {
  type: string;
  content: string;
  line: number;
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

abstract class ParsedWat {
  abstract get type(): "list" | "atom" | "builtin";
  abstract toString(): string;
  abstract expand(scope: ExpansionScope): ParsedWat | ParsedWat[];
}

type BuiltinFn = (
  scope: ExpansionScope,
  args: Map<string, ParsedWat>
) => ParsedWat[];

class Builtin extends ParsedWat {
  private readonly fn_: BuiltinFn;

  constructor(fn: BuiltinFn) {
    super();
    this.fn_ = fn;
  }
  get type(): "list" | "atom" | "builtin" {
    return "builtin";
  }
  toString(): string {
    throw new Error("Method not implemented.");
  }
  expand(scope: ExpansionScope): ParsedWat | ParsedWat[] {
    throw new Error("Method not implemented.");
  }

  expandBuiltin(
    scope: ExpansionScope,
    args: Map<string, ParsedWat>
  ): ParsedWat[] {
    return this.fn_(scope, args);
  }
}

function isBuiltin(value: any): value is Builtin {
  return value && value.type === "builtin";
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

function convertStr(bits: number, str: string): string {
  if (bits != 32 && bits != 64 && bits != 128 && bits != 192) {
    throw new Error(
      `Unknown bit length while converting string: ${bits} ${JSON.stringify(
        str
      )}`
    );
  }

  const buffer = Buffer.from(str, "utf-8");
  if (buffer.length * 8 > bits) {
    throw new Error(
      `String is too long for number of bits: ${bits} ${JSON.stringify(str)}`
    );
  }

  const fullBuffer = new Uint8Array(bits / 8);
  fullBuffer.set(buffer);

  if (bits == 32) {
    const words = new Uint32Array(fullBuffer);
    return `0x${words[0].toString(16)} ${buffer.length}`;
  }
  const dwords = new BigUint64Array(fullBuffer.buffer);
  const array = Array.from(dwords).map((el) => "0x" + el.toString(16));
  array.push(buffer.length.toString());
  return array.join(" ");
}

interface ListOptions {
  isComment?: boolean;
  isMacroScope?: boolean;
}

class List extends ParsedWat {
  private readonly elements_: ParsedWat[];
  private readonly isComment_: boolean;
  private readonly isMacroScope_: boolean;

  constructor(elements: ParsedWat[], { isComment, isMacroScope }: ListOptions) {
    super();
    this.elements_ = elements;
    this.isComment_ = !!isComment;
    this.isMacroScope_ = !!isMacroScope;
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

    return { name: name, args: argList, body: this.trim(body) };
  }

  trim(arr: ParsedWat[]): ParsedWat[] {
    const first = arr.findIndex(
      (v) => !isAtom(v) || !isWhitespaceToken(v.token)
    );
    let last = arr.length - 1;
    while (
      last > first &&
      isAtom(arr[last]) &&
      isWhitespaceToken((arr[last] as Atom).token)
    ) {
      last--;
    }
    return arr.slice(first, last + 1);
  }

  expandMacro(
    scope: ExpansionScope,
    macroDefinition: MacroDefinition
  ): ParsedWat[] {
    const args = Array.from(fluent.skip(this.getSignificantElements(), 1));
    if (args.length != macroDefinition.args.length) {
      throw new Error(
        `Macro ${this.macroName} called with ${args.length} args ${args
          .map((el) => el.toString())
          .join(" ")}, expecting ${macroDefinition.args.length}`
      );
    }
    const argMap: Map<string, ParsedWat> = new Map();
    for (let i = 0; i != args.length; i++) {
      argMap.set(macroDefinition.args[i], args[i]);
    }

    if (macroDefinition.body.length == 1) {
      const first = macroDefinition.body[0];
      if (isBuiltin(first)) {
        return first.expandBuiltin(scope, argMap);
      }
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
    return new List(expanded, {});
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
    } else if (this.isMacroScope_) {
      const atom = this.firstSignificantAtom();
      if (!atom || !atom.token.content.match(/^\w+$/)) {
        throw new Error("Macro Scope must start with a name");
      }
      const content = this.elements.slice(this.elements.indexOf(atom) + 1);
      const name = atom.token.content.toUpperCase();

      return `(; %( begin macro scope ${name} ;)${content
        .map((el) => el.toString())
        .join("")}(; end macro scope ${name} )% ;)`;
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
      return new List([this], { isComment: true });
    } else if (this.isMacro) {
      const name = this.macroName;
      const defn = scope.lookup(name);
      if (defn) {
        return this.expandMacro(scope, defn)
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
      } else {
        throw new Error(
          `Unknown macro ${name} at line ${
            this.firstSignificantAtom()?.token.line
          }`
        );
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

    return new List(expandedElements, { isMacroScope: this.isMacroScope_ });
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

type ListDelimType = "(" | "(;" | "%(" | "NONE";

export function parse(input: string): ParsedWat[] {
  const parsed: ParsedWat[] = [];

  const listStack: ParsedWat[][] = [];
  const typeStack: ListDelimType[] = [];
  let curr = parsed;
  let currType: ListDelimType = "NONE";

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
        const list = new List(curr, {});
        curr = listStack.pop() || [];
        currType = typeStack.pop() || "NONE";
        curr.push(list);
      } else {
        throw new Error(
          `Unmatched (, got ${JSON.stringify(token.content)} at line ${
            token.line
          }`
        );
      }
    } else if (token.content === "(;") {
      listStack.push(curr);
      typeStack.push(currType);
      curr = [];
      currType = "(";
    } else if (token.content === ";)") {
      if (currType === "(;") {
        const list = new List(curr, { isComment: true });
        curr = listStack.pop() || [];
        currType = typeStack.pop() || "NONE";
        curr.push(list);
      } else {
        throw new Error(
          `Unmatched '${currType}', got ${JSON.stringify(
            token.content
          )}, at line ${token.line}`
        );
      }
    } else if (token.content === "%(") {
      listStack.push(curr);
      typeStack.push(currType);
      curr = [];
      currType = "%(";
    } else if (token.content === ")%") {
      if (currType === "%(") {
        const list = new List(curr, { isMacroScope: true });
        curr = listStack.pop() || [];
        currType = typeStack.pop() || "NONE";
        curr.push(list);
      } else {
        throw new Error(
          `Unmatched '${currType}', got ${JSON.stringify(
            token.content
          )}, at line ${token.line}`
        );
      }
    } else {
      throw new Error(
        `Unexpected delimiter ${JSON.stringify(token.content)} at line ${
          token.line
        }`
      );
    }
  }

  if (listStack.length) {
    while (listStack.length) {
      const list = new List(curr, {});
      curr = listStack.pop() || [];
      currType = typeStack.pop() || "NONE";
      curr.push(list);
    }
    // const top = listStack.pop() || [];
    // const first = top.find(isAtom);
    //throw new Error(`Unbalanced paren at line ${first?.token.line}`);
    parsed.push(
      new Atom({
        type: "comment",
        content: `;; %%% ERROR Unbalanced paren %%%`,
        line: 0,
      })
    );
  }

  return parsed;
}

function expandBuiltinArg(
  scope: ExpansionScope,
  arg: ParsedWat | undefined,
  name: string
): Atom {
  if (!arg) {
    throw new Error(`Missing arg: ${name}`);
  }
  if (isAtom(arg)) {
    return arg;
  }
  const expanded = arg.expand(scope);
  if (Array.isArray(expanded)) {
    if (expanded.length == 1 && isAtom(expanded[0])) {
      return expanded[0];
    }
    throw new Error(
      `Invalid expansion of ${name}: ${expanded
        .map((el) => el.toString())
        .join(" ")}`
    );
  } else if (isAtom(expanded)) {
    return expanded;
  } else {
    throw new Error(`Invalid expansion of ${name}: ${expanded.toString()}`);
  }
}

function getVersion() {
  try {
    return child_process
      .execSync("git describe --tags")
      .toString("utf-8")
      .trim();
  } catch (err) {
    console.log(
      `unable to get version information from git: ${
        err instanceof Error ? err.message : (err as any)
      }`
    );
    return "v0.0-0-00000000";
  }
}

export function emit(parsed: ParsedWat[]) {
  const scope = new ExpansionScope();
  scope.add({
    name: "%str",
    args: ["%next", "%bits", "%str"],
    body: [
      new Builtin((scope, args) => {
        const bits = expandBuiltinArg(scope, args.get("%bits"), "%bits");
        const str = expandBuiltinArg(scope, args.get("%str"), "%str");
        const converted = convertStr(
          Number(bits.token.content),
          JSON.parse(str.token.content)
        );
        return [
          new List(
            [
              args.get("%next") as ParsedWat,
              ...converted.split(" ").map(
                (el) =>
                  new Atom({
                    content: el,
                    type: "element",
                    line: str.token.line,
                  })
              ),
            ],
            {}
          ),
        ];
      }),
    ],
  });
  const version = getVersion();
  scope.add({
    name: "%version",
    args: [],
    body: [
      new Builtin((scope, args) => {
        return [
          new Atom({
            content: JSON.stringify(version),
            type: "string",
            line: 0,
          }),
        ];
      }),
    ],
  });

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
