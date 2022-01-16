import * as fluent from "./fluent";
import { Atom, isAtom, isList, List, ParsedWat } from "./parsedwat";
import { isCommentToken, isWhitespaceToken } from "./tokens";

export function validateWat(parsedWat: ParsedWat) {
  if (isAtom(parsedWat)) {
    if (
      !isCommentToken(parsedWat.token) &&
      !isWhitespaceToken(parsedWat.token)
    ) {
      throw new Error(`Unexpected top-level token at ${parsedWat.token.line}`);
    }
    return;
  } else if (isList(parsedWat)) {
    if (parsedWat.isComment) {
      return;
    }

    const first = parsedWat.firstSignificantAtom();

    if (parsedWat.isDefinition) {
      throw new Error(`Unexpected macro definition at ${first?.token.line}`);
    } else if (parsedWat.isMacro) {
      throw new Error(
        `Unexpanded macro ${parsedWat.macroName} at ${first?.token.line}`
      );
    }

    validateModule(parsedWat);
  } else {
    throw new Error("Unexpected top-level parse object");
  }
}

function isAtomStartedList(list: List): Atom {
  const first = Array.from(fluent.take(list.getSignificantElements(), 1));
  if (first.length == 0) {
    throw new Error("Unexpected empty list");
  } else if (!isAtom(first[0])) {
    throw new Error(
      `Expecting list to start with atomm, got ${first[0].toString()}`
    );
  }
  return first[0];
}

function validateModule(module: List) {
  const first = isAtomStartedList(module);
  if (first.token.content !== "module") {
    throw new Error(
      `Expecting module, got ${first.toString()}, at line ${first.token.line}`
    );
  }

  validateModuleLevel(fluent.skip(module.getSignificantElements(), 1));
}

function validateModuleLevel(elements: Generator<ParsedWat>) {
  for (const elem of elements) {
    if (isAtom(elem)) {
      throw new Error(`Unexpected atom in module ${elem.token.line}`);
    } else if (isList(elem)) {
      if (elem.isComment) {
        continue;
      }
      if (elem.isMacroScope) {
        validateModuleLevel(fluent.skip(elem.getSignificantElements(), 1));
        continue;
      }

      const start = isAtomStartedList(elem);
      switch (start.token.content) {
        case "func":
          validateFunction(elem);
          break;
        case "memory":
        case "type":
        case "table":
        case "elem":
        case "global":
        case "start":
        case "export":
          break;
        default:
          throw new Error(
            `Unexpected '${start}' in module at line ${start.token.line}`
          );
      }
    }
  }
}

function assertAtom(elem: ParsedWat | undefined, content: string): Atom {
  if (!elem) {
    throw new Error(`Expecting ${content}, got nothing`);
  }
  if (!isAtom(elem)) {
    throw new Error(`Expecting ${content}, got ${elem}`);
  }
  if (elem.token.content !== content) {
    throw new Error(
      `Expecting ${content}, got ${elem} at line ${elem.token.line}`
    );
  }
  return elem;
}

function assertAtomType(elem: ParsedWat | undefined, type: string) {
  if (!elem) {
    throw new Error(`Expecting ${type}, got nothing`);
  }
  if (!isAtom(elem)) {
    throw new Error(`Expecting ${type}, got ${elem}`);
  }
  if (elem.token.type !== type) {
    throw new Error(`Expecting ${type}, got ${elem}`);
  }
}

function assertAtomMatch(
  elem: ParsedWat | undefined,
  content: RegExp,
  desc?: string
): Atom {
  if (!elem) {
    throw new Error(`Expecting ${desc || content}, got nothing`);
  }
  if (!isAtom(elem)) {
    throw new Error(`Expecting ${desc || content}, got ${elem}`);
  }
  if (!elem.token.content.match(content)) {
    throw new Error(`Expecting ${desc || content}, got ${elem}`);
  }
  return elem;
}

function assertAtomMatchOptional(
  elem: ParsedWat | undefined,
  content: RegExp
): Atom | undefined {
  if (!elem || !isAtom(elem)) {
    return;
  }
  if (elem.token.content.match(content)) {
    return elem;
  }
  return;
}

const kIdentifierReg = /^\$[0-9a-zA-Z!#$%&'*+.\/:<=>?@\\^_`|~-]+$/;
const kNumTypeReg = /^(i32|i64|f32|f64)$/;

function validateFunction(func: List) {
  const elements = Array.from(func.getSignificantElements());
  assertAtom(elements.shift(), "func");
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  let state: "import" | "param" | "result" | "local" | "instr" = "import";
  const locals: string[] = [];

  do {
    const curr = elements.shift();
    if (!isList(curr)) {
      throw new Error(
        `Function ${name} got ${curr}, expecting a list, at line ${name.token.line}`
      );
      continue;
    }
    if (curr.isComment) {
      continue;
    }
    const start = isAtomStartedList(curr);

    if (state === "import") {
      state = "param";
      if (start.token.content === "import") {
        validateImport(name, curr);
        continue;
      }
    }

    if (state === "param") {
      if (start.token.content === "param") {
        validateParam(name, locals, curr);
        continue;
      }
      state = "result";
    }

    if (state === "result") {
      state = "local";
      if (start.token.content === "result") {
        validateResult(name, curr);
        continue;
      }
    }

    if (state === "local") {
      if (start.token.content === "local") {
        validateLocal(name, locals, curr);
        continue;
      }
      state = "instr";
    }

    if (state === "instr") {
      validateInstructions(name, { locals: locals, blocks: [] }, elements);
      return;
    }
  } while (elements.length);
}

function validateParam(funcName: Atom, locals: string[], param: List) {
  validateParamOrLocal(funcName, locals, param, "param");
}

function validateLocal(funcName: Atom, locals: string[], param: List) {
  validateParamOrLocal(funcName, locals, param, "local");
}

function validateParamOrLocal(
  funcName: Atom,
  locals: string[],
  param: List,
  paramOrLocal: "param" | "local"
) {
  const elements = Array.from(param.getSignificantElements());
  assertAtom(elements.shift(), paramOrLocal);
  const second = elements.shift();
  const name = assertAtomMatchOptional(second, kIdentifierReg);
  assertAtomMatch(name ? elements.shift() : second, kNumTypeReg, "numtype");

  if (name) {
    if (locals.some((el) => el === name.token.content)) {
      throw new Error(
        `Duplicate ${paramOrLocal} ${name} in ${funcName} at line ${name.token.line}`
      );
    }
    locals.push(name.token.content);
  }
}

function validateResult(funcName: Atom, result: List) {
  const elements = Array.from(result.getSignificantElements());
  assertAtom(elements.shift(), "result");
  assertAtomMatch(elements.shift(), kNumTypeReg, "numtype");
}

function validateImport(funcName: Atom, imprt: List) {
  const elements = Array.from(imprt.getSignificantElements());
  assertAtom(elements.shift(), "import");
  assertAtomType(elements.shift(), "string");
  assertAtomType(elements.shift(), "string");
}

function validateInstructions(
  funcName: Atom,
  context: ValidatorContext,
  instructions: ParsedWat[]
) {
  instructions.forEach((instr) =>
    validateInstruction(funcName, context, instr)
  );
}

function validateInstruction(
  funcName: Atom,
  context: ValidatorContext,
  instruction: ParsedWat
) {
  if (isAtom(instruction)) {
    // TODO validate non s-expression
    console.warn(
      `Instruction '${instruction}' not in S-Experession form in ${funcName} at line ${instruction.token.line}`
    );
  } else if (isList(instruction)) {
    if (instruction.isComment) {
      return;
    }
    const mnem = isAtomStartedList(instruction);
    const validator = lookupInstructionValidator(funcName, mnem);
    validator(funcName, context, instruction);
  } else {
    throw new Error(`unexpected type ${instruction} in ${funcName}`);
  }
}

interface ValidatorContext {
  locals: string[];
  blocks: string[];
}
type Validator = (funcName: Atom, locals: ValidatorContext, list: List) => void;

const kInstructionValidators: Map<string, Validator> = new Map([
  ["block", blockValidator],
  ["br", brValidator],
  ["br_if", brifValidator],
  ["call", callValidator],
  ["call_indirect", callValidator],
  ["f32.const", constValidator],
  ["f64.const", constValidator],
  ["global.get", globalGetValidator],
  ["global.set", globalSetValidator],
  ["i32.const", constValidator],
  ["i32.load", loadValidator],
  ["i32.store", storeValidator],
  ["i64.const", constValidator],
  ["i64.load", loadValidator],
  ["i64.store", storeValidator],
  ["if", ifValidator],
  ["local.get", localGetValidator],
  ["local.set", localSetValidator],
  ["local.tee", localSetValidator],
  ["loop", blockValidator],
  ["memory.size", thunkValidator],
  ["nop", thunkValidator],
  ["return", returnValidator],
  ["select", selectValidator],
  ["type", typeValidator],
  ["unreachable", thunkValidator],
]);

const kUnaryInstructions = [
  "drop",
  "f32.abs",
  "f32.ceil",
  "f32.convert_i32_s",
  "f32.convert_i32_u",
  "f32.convert_i64_s",
  "f32.convert_i64_u",
  "f32.demote_f64",
  "f32.eqz",
  "f32.floor",
  "f32.max",
  "f32.min",
  "f32.nearest",
  "f32.neg",
  "f32.reinterpret_i32",
  "f32.reinterpret_i64",
  "f32.sqrt",
  "f32.trunc",
  "f64.abs",
  "f64.ceil",
  "f64.convert_i32_s",
  "f64.convert_i32_u",
  "f64.convert_i64_s",
  "f64.convert_i64_u",
  "f64.eqz",
  "f64.floor",
  "f64.max",
  "f64.min",
  "f64.nearest",
  "f64.neg",
  "f64.promote_f32",
  "f64.reinterpret_i64",
  "f64.sqrt",
  "f64.trunc",
  "i32.clz",
  "i32.ctz",
  "i32.eqz",
  "i32.popcnt",
  "i32.reinterpret_f32",
  "i32.trunc_f32_s",
  "i32.trunc_f32_u",
  "i32.trunc_f64_s",
  "i32.trunc_f64_u",
  "i32.trunc_sat_f32_s",
  "i32.trunc_sat_f32_u",
  "i32.trunc_sat_f64_s",
  "i32.trunc_sat_f64_u",
  "i32.wrap_i64",
  "i64.clz",
  "i64.ctz",
  "i64.eqz",
  "i64.extend_i32_s",
  "i64.extend_i32_u",
  "i64.popcnt",
  "i64.reinterpret_f64",
  "i64.trunc_f32_s",
  "i64.trunc_f32_u",
  "i64.trunc_f64_s",
  "i64.trunc_f64_u",
  "i64.trunc_sat_f32_s",
  "i64.trunc_sat_f32_u",
  "i64.trunc_sat_f64_s",
  "i64.trunc_sat_f64_u",
  "memory.grow",
];

kUnaryInstructions.forEach((el) =>
  kInstructionValidators.set(el, unaryValidator)
);

const kBinaryInstructions = [
  "f32.add",
  "f32.div",
  "f32.eq",
  "f32.ge",
  "f32.gt",
  "f32.le",
  "f32.lt",
  "f32.mul",
  "f32.ne",
  "f32.sub",
  "f64.add",
  "f64.div",
  "f64.eq",
  "f64.ge",
  "f64.gt",
  "f64.le",
  "f64.lt",
  "f64.mul",
  "f64.ne",
  "f64.sub",
  "i32.add",
  "i32.and",
  "i32.div_s",
  "i32.div_u",
  "i32.eq",
  "i32.ge_s",
  "i32.ge_u",
  "i32.gt_s",
  "i32.gt_u",
  "i32.le_s",
  "i32.le_u",
  "i32.lt_s",
  "i32.lt_u",
  "i32.mul",
  "i32.ne",
  "i32.or",
  "i32.rem_s",
  "i32.rem_u",
  "i32.rotl",
  "i32.rotr",
  "i32.shl",
  "i32.shr_s",
  "i32.shr_u",
  "i32.sub",
  "i32.xor",
  "i64.add",
  "i64.and",
  "i64.div_s",
  "i64.div_u",
  "i64.eq",
  "i64.ge_s",
  "i64.ge_u",
  "i64.gt_s",
  "i64.gt_u",
  "i64.le_s",
  "i64.le_u",
  "i64.lt_s",
  "i64.lt_u",
  "i64.mul",
  "i64.ne",
  "i64.or",
  "i64.rem_s",
  "i64.rem_u",
  "i64.rotl",
  "i64.rotr",
  "i64.shl",
  "i64.shr_s",
  "i64.shr_u",
  "i64.sub",
  "i64.xor",
];

kBinaryInstructions.forEach((el) =>
  kInstructionValidators.set(el, binaryValidator)
);

const kLoadInstructions = [
  "f32.load",
  "f64.load",
  "i32.load",
  "i32.load16_s",
  "i32.load16_u",
  "i32.load8_s",
  "i32.load8_u",
  "i64.load",
  "i64.load16_s",
  "i64.load16_u",
  "i64.load32_s",
  "i64.load32_u",
  "i64.load8_s",
  "i64.load8_u",
];

kLoadInstructions.forEach((el) =>
  kInstructionValidators.set(el, loadValidator)
);

const kStoreInstructions = [
  "f32.store",
  "f64.store",
  "i32.store",
  "i32.store16",
  "i32.store8",
  "i64.store",
  "i64.store16",
  "i64.store32",
  "i64.store8",
];

kStoreInstructions.forEach((el) =>
  kInstructionValidators.set(el, storeValidator)
);

function lookupInstructionValidator(funcName: Atom, instr: Atom) {
  const validator = kInstructionValidators.get(instr.token.content);
  if (!validator) {
    throw new Error(
      `Unknown instruction ${instr} in ${funcName} at line ${instr.token.line}`
    );
  }
  return validator;
}

function localGetValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
) {
  getValidator(funcName, context, list, true);
}

function globalGetValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
) {
  getValidator(funcName, context, list, false);
}

function getValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List,
  checkLocals: boolean
) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift();
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");
  if (checkLocals && !context.locals.some((el) => el === name.token.content)) {
    throw new Error(
      `${first} has unknown identifier ${name} in ${funcName} at line ${name.token.line}`
    );
  }
  if (elements.length !== 0) {
    throw new Error(
      `${first} should have 1 argument, found ${
        1 + elements.length
      } in ${funcName} at line ${name.token.line}`
    );
  }
}

function localSetValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
) {
  setValidator(funcName, context, list, true);
}

function globalSetValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
) {
  setValidator(funcName, context, list, false);
}

function setValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List,
  checkLocals: boolean
) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift();
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");
  if (checkLocals && !context.locals.some((el) => el === name.token.content)) {
    throw new Error(
      `${first} has unknown identifier ${name} in ${funcName} at line ${name.token.line}`
    );
  }
  if (elements.length !== 1) {
    throw new Error(
      `${first} should have 2 arguments, found ${
        1 + elements.length
      } in ${funcName} at line ${name.token.line}`
    );
  }
  validateInstruction(funcName, context, elements[0]);
}

function constValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift();
  if (elements.length !== 1) {
    throw new Error(
      `${first} should have 1 argument, found ${
        1 + elements.length
      } in ${funcName} at line ${(first as Atom).token.line}`
    );
  }
  const constant = elements.shift();
  if (!isAtom(constant)) {
    throw new Error(
      `${first} should have constant argument, found ${constant} in ${funcName} at line ${
        (first as Atom).token.line
      }`
    );
  }
}

function loadValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const offset = assertAtomMatchOptional(elements[0], /^offset=\d+$/);
  if (offset) {
    elements.shift();
  }
  if (elements.length != 1) {
    throw new Error(
      `${first} should have 1 argument, found ${elements.length} in ${funcName} at line ${first.token.line}`
    );
  }
  validateInstruction(funcName, context, elements[0]);
}

function storeValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const offset = assertAtomMatchOptional(elements[0], /^offset=\d+$/);
  if (offset) {
    elements.shift();
  }
  if (elements.length != 2) {
    throw new Error(
      `${first} should have 2 arguments, found ${elements.length} in ${funcName} at line ${first.token.line}`
    );
  }
  validateInstruction(funcName, context, elements[0]);
  validateInstruction(funcName, context, elements[1]);
}

function callValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  validateInstructions(funcName, context, elements);
}

function returnValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
) {
  const elements = Array.from(list.getSignificantElements());
  const ret = assertAtom(elements.shift(), "return");
  if (elements.length == 0) {
    return;
  }
  if (elements.length > 1) {
    throw new Error(
      `return should have at most 1 argument, found ${
        1 + elements.length
      } in ${funcName} at line ${ret.token.line}`
    );
  }

  validateInstruction(funcName, context, elements[0]);
}

function ifValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const key = assertAtom(elements.shift(), "if");
  const test = elements.shift();
  if (!test || !isList(test)) {
    throw new Error(
      `Expecting (<test> ...) in if in ${funcName} at line ${key.token.line}`
    );
  }
  validateInstruction(funcName, context, test);

  const then = elements.shift();
  if (!then || !isList(then)) {
    throw new Error(
      `Expecting (then ...) in if in ${funcName} at line ${key.token.line}`
    );
  }
  const thenElements = Array.from(then.getSignificantElements());
  assertAtom(thenElements.shift(), "then");
  validateInstructions(funcName, context, thenElements);

  if (elements.length == 0) {
    return;
  }

  const elseElement = elements.shift();
  if (!elseElement || !isList(elseElement)) {
    throw new Error(
      `Expecting (else ...) in if in ${funcName} at line ${key.token.line}`
    );
  }
  const elseElements = Array.from(elseElement.getSignificantElements());
  assertAtom(elseElements.shift(), "else");
  validateInstructions(funcName, context, elseElements);

  if (elements.length == 0) {
    return;
  }

  throw new Error(
    `if should have at most 3 arguments, found ${
      2 + elements.length
    } in ${funcName} at line ${key.token.line}`
  );
}

function thunkValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;

  if (elements.length != 0) {
    throw new Error(
      `${first} should have no, found ${elements.length} in ${funcName} at line ${first.token.line}`
    );
  }
}

function unaryValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;

  if (elements.length != 1) {
    throw new Error(
      `${first} should have 1 argument, found ${elements.length} in ${funcName} at line ${first.token.line}`
    );
  }

  validateInstruction(funcName, context, elements[0]);
}

function binaryValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;

  if (elements.length != 2) {
    throw new Error(
      `${first} should have 2 arguments, found ${elements.length} in ${funcName} at line ${first.token.line}`
    );
  }

  validateInstruction(funcName, context, elements[0]);
  validateInstruction(funcName, context, elements[1]);
}

function selectValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
) {
  const elements = Array.from(list.getSignificantElements());
  const first = assertAtom(elements.shift(), "select");

  if (elements.length != 3) {
    throw new Error(
      `${first} should have 3 arguments, found ${elements.length} in ${funcName} at line ${first.token.line}`
    );
  }

  validateInstruction(funcName, context, elements[0]);
  validateInstruction(funcName, context, elements[1]);
  validateInstruction(funcName, context, elements[2]);
}

function blockValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  if (context.blocks.some((el) => el === name.token.content)) {
    throw new Error(
      `${first} has duplicate label ${name} in ${funcName} at line ${name.token.line}`
    );
  }

  const blockContext = {
    locals: context.locals,
    blocks: [...context.blocks, name.token.content],
  };

  validateInstructions(funcName, blockContext, elements);
}

function brifValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  if (!context.blocks.some((el) => el === name.token.content)) {
    throw new Error(
      `${first} has unknown label ${name} in ${funcName} at line ${name.token.line}`
    );
  }

  if (elements.length != 1) {
    throw new Error(
      `${first} should have 1 argument, found ${elements.length} in ${funcName} at line ${first.token.line}`
    );
  }

  validateInstruction(funcName, context, elements[0]);
}

function brValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  if (!context.blocks.some((el) => el === name.token.content)) {
    throw new Error(
      `${first} has unknown label ${name} in ${funcName} at line ${name.token.line}`
    );
  }

  if (elements.length) {
    throw new Error(
      `${first} should have no arguments, found ${elements.length} in ${funcName} at line ${first.token.line}`
    );
  }
}

function typeValidator(funcName: Atom, context: ValidatorContext, list: List) {
  const elements = Array.from(list.getSignificantElements());
  const first = assertAtom(elements.shift(), "type");
  assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  if (elements.length) {
    throw new Error(
      `${first} should have no arguments, found ${elements.length} in ${funcName} at line ${first.token.line}`
    );
  }
}
