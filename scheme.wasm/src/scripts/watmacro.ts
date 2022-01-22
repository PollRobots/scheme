import * as fluent from "./fluent";
import child_process from "child_process";
import { validateWat } from "./validatewat";
import {
  ParsedWat,
  Atom,
  List,
  ExpansionScope,
  Builtin,
  convertStr,
  isAtom,
} from "./parsedwat";
import { tokenize, isDelimiterToken } from "./tokens";

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
    //throw new Error(`Unbalanced paren at line ${first?.line}`);
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

function stringify(parsed: ParsedWat, validate: boolean): string {
  if (validate) {
    validateWat(parsed);
  }
  return parsed.toString();
}

export function emit(parsed: ParsedWat[], validate: boolean) {
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
                    line: str.line,
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
        output.push(stringify(el, validate));
      });
    } else {
      output.push(stringify(expanded, validate));
    }
  }

  return output;
}
