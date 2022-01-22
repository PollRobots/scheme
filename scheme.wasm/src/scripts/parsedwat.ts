import * as fluent from "./fluent";
import {
  isCommentToken,
  isDefineToken,
  isMacroToken,
  isWhitespaceToken,
  MacroToken,
  Token,
} from "./tokens";

export abstract class ParsedWat {
  abstract get type(): "list" | "atom" | "builtin";
  abstract get line(): number;
  abstract toString(): string;
  abstract expand(scope: ExpansionScope): ParsedWat | ParsedWat[];
}

type BuiltinFn = (
  scope: ExpansionScope,
  args: Map<string, ParsedWat>
) => ParsedWat[];

export class Builtin extends ParsedWat {
  private readonly fn_: BuiltinFn;

  constructor(fn: BuiltinFn) {
    super();
    this.fn_ = fn;
  }

  get type(): "list" | "atom" | "builtin" {
    return "builtin";
  }

  get line() {
    return 0;
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

export class ExpansionScope {
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

export function convertStr(bits: number, str: string): string {
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

export class List extends ParsedWat {
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

  get line() {
    const atom = this.firstSignificantAtom();
    return atom ? atom.line : 0;
  }

  get elements(): ParsedWat[] {
    return this.elements_;
  }

  get isComment(): boolean {
    return this.isComment_;
  }

  get isMacroScope(): boolean {
    return this.isMacroScope_;
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

  firstSignificantAtom(): Atom | undefined {
    const iter = this.getSignificantElements();
    const first = iter.next();
    if (!first.done && isAtom(first.value)) {
      return first.value;
    }
  }

  *getSignificantElements(): Generator<ParsedWat> {
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
          `Unknown macro ${name} at line ${this.firstSignificantAtom()?.line}`
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

export function isList(value: any): value is List {
  return value && value.type === "list";
}

export class Atom extends ParsedWat {
  private readonly token_: Token;

  constructor(token: Token) {
    super();
    this.token_ = token;
  }

  get type(): "list" | "atom" {
    return "atom";
  }

  get line() {
    return this.token_.line;
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

export function isAtom(value: any): value is Atom {
  return value && value.type === "atom";
}
