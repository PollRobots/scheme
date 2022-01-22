import * as fluent from "./fluent";
import { Atom, isAtom, isList, List, ParsedWat } from "./parsedwat";
import { isCommentToken, isWhitespaceToken } from "./tokens";

type WatType = "i32" | "i64" | "f32" | "f64" | "any" | "none";

interface FunctionSignature {
  name: string;
  params: WatType[];
  result: WatType[];
}

interface LocalDeclaration {
  name: string;
  type: WatType;
}

interface GlobalDeclaration extends LocalDeclaration {
  mutable: boolean;
}

interface ValidatorContext {
  functions: Map<string, FunctionSignature>;
  locals: Map<string, LocalDeclaration>;
  globals: Map<string, GlobalDeclaration>;
  blocks: string[];
}

type Validator = (
  funcName: Atom,
  locals: ValidatorContext,
  list: List
) => WatType[];

export function validateWat(parsedWat: ParsedWat) {
  if (isAtom(parsedWat)) {
    if (
      !isCommentToken(parsedWat.token) &&
      !isWhitespaceToken(parsedWat.token)
    ) {
      throw new Error(`Unexpected top-level token at ${parsedWat.line}`);
    }
    return;
  } else if (isList(parsedWat)) {
    if (parsedWat.isComment) {
      return;
    }

    const first = parsedWat.firstSignificantAtom();

    if (parsedWat.isDefinition) {
      throw new Error(`Unexpected macro definition at ${parsedWat.line}`);
    } else if (parsedWat.isMacro) {
      throw new Error(
        `Unexpanded macro ${parsedWat.macroName} at ${parsedWat.line}`
      );
    }

    validateModule(parsedWat);
  } else {
    throw new Error(`Unexpected top-level parse object at ${parsedWat.line}`);
  }
}

function isAtomStartedList(list: List): Atom {
  const first = Array.from(fluent.take(list.getSignificantElements(), 1));
  if (first.length == 0) {
    throw new Error("Unexpected empty list");
  } else if (!isAtom(first[0])) {
    throw new Error(
      `Expecting list to start with atom, got ${first[0].toString()} at ${
        list.line
      }`
    );
  }
  return first[0];
}

function validateModule(module: List) {
  const first = isAtomStartedList(module);
  if (first.token.content !== "module") {
    throw new Error(
      `Expecting module, got ${first.toString()}, at ${first.line}`
    );
  }
  const functions = new Map<string, FunctionSignature>();
  const globals = new Map<string, GlobalDeclaration>();
  extractFunctionSignatures(
    fluent.skip(module.getSignificantElements(), 1),
    functions,
    globals
  );
  validateModuleLevel(
    fluent.skip(module.getSignificantElements(), 1),
    functions,
    globals
  );
}

function extractFunctionSignatures(
  elements: Generator<ParsedWat>,
  functions: Map<string, FunctionSignature>,
  globals: Map<string, GlobalDeclaration>
) {
  for (const elem of elements) {
    if (isList(elem)) {
      if (elem.isComment) {
        continue;
      } else if (elem.isMacroScope) {
        extractFunctionSignatures(
          fluent.skip(elem.getSignificantElements(), 1),
          functions,
          globals
        );
        continue;
      }

      const start = isAtomStartedList(elem);
      if (start.token.content === "func") {
        const signature = extractFunctionSignature(elem);
        functions.set(signature.name, signature);
      } else if (start.token.content === "global") {
        validateGlobal(elem, globals);
      }
    }
  }
}

function extractFunctionSignature(func: List): FunctionSignature {
  const elements = Array.from(func.getSignificantElements());
  assertAtom(elements.shift(), "func");
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  let state: "import" | "param" | "result" | "local" | "instr" = "import";
  const locals = new Map<string, LocalDeclaration>();
  const paramTypes: WatType[] = [];
  const resultTypes: WatType[] = [];

  do {
    const curr = elements.shift();
    if (!isList(curr)) {
      throw new Error(
        `Function ${name} got ${curr}, expecting a list, at ${name.line}`
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
        paramTypes.push(getSignatureType(curr, "param"));
        continue;
      }
      state = "result";
    }

    if (state === "result") {
      if (start.token.content === "result") {
        validateResult(name, curr);
        resultTypes.push(getSignatureType(curr, "result"));
      }
      break;
    }
  } while (elements.length);

  return {
    name: name.token.content,
    params: paramTypes,
    result: resultTypes,
  };
}

function validateModuleLevel(
  elements: Generator<ParsedWat>,
  functions: Map<string, FunctionSignature>,
  globals: Map<string, GlobalDeclaration>
) {
  for (const elem of elements) {
    if (isAtom(elem)) {
      throw new Error(`Unexpected atom in module ${elem.line}`);
    } else if (isList(elem)) {
      if (elem.isComment) {
        continue;
      }
      if (elem.isMacroScope) {
        validateModuleLevel(
          fluent.skip(elem.getSignificantElements(), 1),
          functions,
          globals
        );
        continue;
      }

      const start = isAtomStartedList(elem);
      switch (start.token.content) {
        case "func":
          validateFunction(elem, functions, globals);
          break;
        case "global":
          // already validated
          break;
        case "memory":
        case "type":
        case "table":
        case "elem":
        case "start":
        case "export":
          break;
        default:
          throw new Error(`Unexpected '${start}' in module at ${start.line}`);
      }
    }
  }
}

function assertAtom(elem: ParsedWat | undefined, content: string): Atom {
  if (!elem) {
    throw new Error(`Expecting ${content}, got nothing`);
  }
  if (!isAtom(elem)) {
    throw new Error(`Expecting ${content}, got ${elem} at ${elem.line}`);
  }
  if (elem.token.content !== content) {
    throw new Error(`Expecting ${content}, got ${elem} at ${elem.line}`);
  }
  return elem;
}

function assertAtomType(elem: ParsedWat | undefined, type: string) {
  if (!elem) {
    throw new Error(`Expecting ${type}, got nothing`);
  }
  if (!isAtom(elem)) {
    throw new Error(`Expecting ${type}, got ${elem} at ${elem.line}`);
  }
  if (elem.token.type !== type) {
    throw new Error(`Expecting ${type}, got ${elem} at ${elem.line}`);
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
    throw new Error(
      `Expecting ${desc || content}, got ${elem} at ${elem.line}`
    );
  }
  if (!elem.token.content.match(content)) {
    throw new Error(
      `Expecting ${desc || content}, got ${elem} at ${elem.line}`
    );
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

function validateGlobal(global: List, globals: Map<string, GlobalDeclaration>) {
  const elements = Array.from(global.getSignificantElements());
  const first = elements.shift() as Atom;
  assertAtom(first, "global");
  if (elements.length != 3) {
    throw new Error(
      `Invalid global declaration at ${
        global.line
      }, expecting 4 elements, got ${1 + elements.length} at ${global.line}`
    );
  }
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");
  if (globals.has(name.token.content)) {
    throw new Error(`Duplicate global ${name} at ${global.line}`);
  }

  const rawType = elements.shift() as ParsedWat;
  let type: WatType;
  let mutable = false;
  if (isAtom(rawType)) {
    type = assertAtomMatch(rawType, kNumTypeReg, "type").token
      .content as WatType;
  } else if (isList(rawType)) {
    const typeElements = Array.from(rawType.getSignificantElements());
    if (typeElements.length !== 2) {
      throw new Error(
        `Invalid global declaration at ${global.line}, expecting 2 elements in type, got ${typeElements.length} at ${global.line}`
      );
    }
    assertAtom(typeElements.shift(), "mut");
    type = assertAtomMatch(typeElements.shift(), kNumTypeReg, "type").token
      .content as WatType;
    mutable = true;
  } else {
    throw new Error(
      `Unexpected type ${rawType} in global ${name} declaration at ${global.line}`
    );
  }

  const initializer = elements.shift();
  if (isList(initializer)) {
    const constType = constValidator(
      first,
      { functions: new Map(), locals: new Map(), globals: globals, blocks: [] },
      initializer
    );
    if (constType.length !== 1) {
      throw new Error(`Unexpected type from const validator at ${global.line}`);
    }
    if (constType[0] != type) {
      throw new Error(
        `Mismatched initializer ${initializer}, expecting ${type}.const, in global ${name} declaration at ${global.line}`
      );
    }
  } else {
    throw new Error(
      `Unexpected initializer ${initializer} in global ${name} declaration at ${global.line}`
    );
  }

  globals.set(name.token.content, {
    name: name.token.content,
    mutable: mutable,
    type: type,
  });
}

function validateFunction(
  func: List,
  functions: Map<string, FunctionSignature>,
  globals: Map<string, GlobalDeclaration>
) {
  const elements = Array.from(func.getSignificantElements());
  assertAtom(elements.shift(), "func");
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  let state: "import" | "param" | "result" | "local" | "instr" = "import";
  const locals = new Map<string, LocalDeclaration>();

  do {
    const curr = elements.shift();
    if (!isList(curr)) {
      throw new Error(
        `Function ${name} got ${curr}, expecting a list, at ${name.line}`
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
      validateInstructions(
        name,
        { functions: functions, locals: locals, globals: globals, blocks: [] },
        elements
      );
      return;
    }
  } while (elements.length);
}

function validateParam(
  funcName: Atom,
  locals: Map<string, LocalDeclaration>,
  param: List
) {
  validateParamOrLocal(funcName, locals, param, "param");
}

function validateLocal(
  funcName: Atom,
  locals: Map<string, LocalDeclaration>,
  param: List
) {
  validateParamOrLocal(funcName, locals, param, "local");
}

function validateParamOrLocal(
  funcName: Atom,
  locals: Map<string, LocalDeclaration>,
  param: List,
  paramOrLocal: "param" | "local"
) {
  const elements = Array.from(param.getSignificantElements());
  assertAtom(elements.shift(), paramOrLocal);
  const second = elements.shift();
  const name = assertAtomMatchOptional(second, kIdentifierReg);
  const type = (name ? elements.shift() : second) as Atom;
  assertAtomMatch(type, kNumTypeReg, "numtype");

  if (name) {
    if (locals.has(name.token.content)) {
      throw new Error(
        `Duplicate ${paramOrLocal} ${name} in ${funcName} at ${name.line}`
      );
    }
    locals.set(name.token.content, {
      name: name.token.content,
      type: type.token.content as WatType,
    });
  }
}

function getSignatureType(
  param: List,
  paramOrResult: "param" | "result"
): WatType {
  const elements = Array.from(param.getSignificantElements());
  assertAtom(elements.shift(), paramOrResult);
  const second = elements.shift();
  const name =
    paramOrResult === "param"
      ? assertAtomMatchOptional(second, kIdentifierReg)
      : undefined;
  const type = (name ? elements.shift() : second) as Atom;
  assertAtomMatch(type, kNumTypeReg, "numtype");
  // we can make this type assertion because it is verified by kNumTypeReg
  return type.token.content as WatType;
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
    validateInstruction(funcName, context, instr, "none")
  );
}

function validateInstruction(
  funcName: Atom,
  context: ValidatorContext,
  instruction: ParsedWat,
  expectedType: WatType
) {
  if (isAtom(instruction)) {
    // TODO validate non s-expression
    console.warn(
      `Instruction '${instruction}' not in S-Experession form in ${funcName} at ${instruction.line}`
    );
  } else if (isList(instruction)) {
    if (instruction.isComment) {
      return;
    }
    const mnem = isAtomStartedList(instruction);
    const validator = lookupInstructionValidator(funcName, mnem);
    const resultType = validator(funcName, context, instruction);
    switch (expectedType) {
      case "none":
        if (resultType.length != 0) {
          console.warn(
            `Expecting ${mnem} not to push, it put ${resultType.join(
              ", "
            )} on the stack in ${funcName} at ${mnem.line}`
          );
        }
        break;
      case "any":
        break;
      default:
        if (resultType.length != 1) {
          console.warn(
            `Expecting ${mnem} to push ${expectedType} onto the stack, in ${funcName} at ${mnem.line}`
          );
        } else if (resultType[0] === "any") {
          // we don't know what type this instruction generated, so we'll assume it is ok
        } else if (resultType[0] !== expectedType) {
          console.warn(
            `Expecting ${mnem} to push ${expectedType} onto the stack, got ${resultType[0]}, in ${funcName} at ${mnem.line}`
          );
        }
    }
  } else {
    throw new Error(`unexpected type ${instruction} in ${funcName}`);
  }
}

const kInstructionValidators: Map<string, Validator> = new Map([
  ["block", blockValidator],
  ["br", brValidator],
  ["br_if", brifValidator],
  ["call", callValidator],
  ["call_indirect", callIndirectValidator],
  ["f32.const", constValidator],
  ["f64.const", constValidator],
  ["global.get", globalGetValidator],
  ["global.set", globalSetValidator],
  ["i32.const", constValidator],
  ["i64.const", constValidator],
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

kUnaryInstructions.forEach((el) => {
  if (el === "drop") {
    kInstructionValidators.set(el, (funcName, context, list) =>
      unaryValidator(funcName, context, list, "any", "none")
    );
    return;
  } else if (el === "memory.grow") {
    kInstructionValidators.set(el, (funcName, context, list) =>
      unaryValidator(funcName, context, list, "i32", "i32")
    );
    return;
  }
  const parts = el.split(".");
  if (parts.length === 1 || !parts[0].match(kNumTypeReg)) {
    throw new Error(`Unexpected unary instruction ${el}`);
  }
  const inputMatch = parts[1].match(/^(?:[\w]*_)*(i32|i64|f32|f64)(?:_[\w]*)*/);
  if (inputMatch) {
    kInstructionValidators.set(el, (funcName, context, list) =>
      unaryValidator(
        funcName,
        context,
        list,
        inputMatch[1] as WatType,
        parts[0] as WatType
      )
    );
  } else {
    kInstructionValidators.set(el, (funcName, context, list) =>
      unaryValidator(
        funcName,
        context,
        list,
        parts[0] as WatType,
        parts[1] === "eqz" ? "i32" : (parts[0] as WatType)
      )
    );
  }
});

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
      `Unknown instruction ${instr} in ${funcName} at ${instr.line}`
    );
  }
  return validator;
}

function localGetValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  return getValidator(funcName, context, list, true);
}

function globalGetValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  // TODO parse globals to get their types
  return getValidator(funcName, context, list, false);
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
  let type: WatType = "any";
  if (checkLocals) {
    const local = context.locals.get(name.token.content);
    if (!local) {
      throw new Error(
        `${first} has unknown identifier ${name} in ${funcName} at ${name.line}`
      );
    }
    type = local.type;
  } else {
    const global = context.globals.get(name.token.content);
    if (!global) {
      throw new Error(
        `${first} has unknown identifier ${name} in ${funcName} at ${name.line}`
      );
    }
    type = global.type;
  }
  if (elements.length !== 0) {
    throw new Error(
      `${first} should have 1 argument, found ${
        1 + elements.length
      } in ${funcName} at ${name.line}`
    );
  }

  return [type];
}

function localSetValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  return setValidator(funcName, context, list, true);
}

function globalSetValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  return setValidator(funcName, context, list, false);
}

function setValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List,
  checkLocals: boolean
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");
  let type: WatType = "any";
  if (checkLocals) {
    const local = context.locals.get(name.token.content);
    if (!local) {
      throw new Error(
        `${first} has unknown identifier ${name} in ${funcName} at ${name.line}`
      );
    }
    type = local.type;
  } else {
    const global = context.globals.get(name.token.content);
    if (!global) {
      throw new Error(
        `${first} has unknown identifier ${name} in ${funcName} at ${name.line}`
      );
    }
    if (!global.mutable) {
      throw new Error(
        `Cannot write to immtable global ${name} in ${funcName} at ${name.line}`
      );
    }
    type = global.type;
  }
  if (elements.length !== 1) {
    throw new Error(
      `${first} should have 2 arguments, found ${
        1 + elements.length
      } in ${funcName} at ${name.line}`
    );
  }
  validateInstruction(funcName, context, elements[0], type);

  if (first.toString().endsWith(".tee")) {
    return [type];
  }
  return [];
}

function constValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  if (elements.length !== 1) {
    throw new Error(
      `${first} should have 1 argument, found ${
        1 + elements.length
      } in ${funcName} at ${first.line}`
    );
  }
  const constant = elements.shift();
  if (!isAtom(constant)) {
    throw new Error(
      `${first} should have constant argument, found ${constant} in ${funcName} at ${first.line}`
    );
  }
  const parts = first.toString().split(".");
  if (
    parts.length !== 2 ||
    !parts[0].match(kNumTypeReg) ||
    parts[1] !== "const"
  ) {
    throw new Error(
      `${first} cannot be validated using the const validator in ${funcName} at ${first.line}`
    );
  }
  return [parts[0] as WatType];
}

function loadValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const offset = assertAtomMatchOptional(elements[0], /^offset=\d+$/);
  if (offset) {
    elements.shift();
  }
  if (elements.length != 1) {
    throw new Error(
      `${first} should have 1 argument, found ${elements.length} in ${funcName} at ${first.line}`
    );
  }
  validateInstruction(funcName, context, elements[0], "i32");
  const parts = first.toString().split(".");
  if (parts.length !== 2 || !parts[0].match(kNumTypeReg)) {
    throw new Error(
      `${first} cannot be validated using the load validator in ${funcName} at ${first.line}`
    );
  }
  return [parts[0] as WatType];
}

function storeValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const offset = assertAtomMatchOptional(elements[0], /^offset=\d+$/);
  if (offset) {
    elements.shift();
  }
  if (elements.length != 2) {
    throw new Error(
      `${first} should have 2 arguments, found ${elements.length} in ${funcName} at ${first.line}`
    );
  }
  validateInstruction(funcName, context, elements[0], "i32");
  const parts = first.toString().split(".");
  if (parts.length !== 2 || !parts[0].match(kNumTypeReg)) {
    throw new Error(
      `${first} cannot be validated using the store validator in ${funcName} at ${first.line}`
    );
  }
  validateInstruction(funcName, context, elements[1], parts[0] as WatType);

  return [];
}

function callValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const name = elements.shift() as Atom;
  assertAtomMatch(name, kIdentifierReg, "identifier");
  if (!context.functions.has(name.token.content)) {
    throw new Error(
      `Calling unknown function ${name} in ${funcName} at ${name.line}`
    );
  }

  const signature = context.functions.get(
    name.token.content
  ) as FunctionSignature;
  if (signature.params.length !== elements.length) {
    throw new Error(
      `Call to function ${name} in ${funcName} has ${elements.length} arguments, expecting ${signature.params.length}. at ${name.line}`
    );
  }

  for (const [element, paramType] of fluent.zip(elements, signature.params)) {
    validateInstruction(funcName, context, element, paramType);
  }
  return signature.result;
}

function callIndirectValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  elements.forEach((instr) =>
    validateInstruction(funcName, context, instr, "any")
  );
  return ["any"];
}

function returnValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const signature = context.functions.get(funcName.token.content);
  if (!signature) {
    throw new Error(
      `Cannot find function declaration for ${funcName} at ${list.line}`
    );
  }
  const elements = Array.from(list.getSignificantElements());
  const ret = assertAtom(elements.shift(), "return");
  if (elements.length != signature.result.length) {
    throw new Error(
      `return should have ${signature.result.length} arguments, found ${elements.length} in ${funcName} at ${ret.line}`
    );
  }
  for (const [element, returnType] of fluent.zip(elements, signature.result)) {
    validateInstruction(funcName, context, element, returnType);
  }

  return [];
}

function ifValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const key = assertAtom(elements.shift(), "if");
  const test = elements.shift();
  if (!test || !isList(test)) {
    throw new Error(
      `Expecting (<test> ...) in if in ${funcName} at ${key.line}`
    );
  }
  validateInstruction(funcName, context, test, "i32");

  const then = elements.shift();
  if (!then || !isList(then)) {
    throw new Error(`Expecting (then ...) in if in ${funcName} at ${key.line}`);
  }
  const thenElements = Array.from(then.getSignificantElements());
  assertAtom(thenElements.shift(), "then");
  validateInstructions(funcName, context, thenElements);

  if (elements.length == 0) {
    return [];
  }

  const elseElement = elements.shift();
  if (!elseElement || !isList(elseElement)) {
    throw new Error(`Expecting (else ...) in if in ${funcName} at ${key.line}`);
  }
  const elseElements = Array.from(elseElement.getSignificantElements());
  assertAtom(elseElements.shift(), "else");
  validateInstructions(funcName, context, elseElements);

  if (elements.length == 0) {
    return [];
  }

  throw new Error(
    `if should have at most 3 arguments, found ${
      2 + elements.length
    } in ${funcName} at ${key.line}`
  );
}

function thunkValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;

  if (elements.length != 0) {
    throw new Error(
      `${first} should have no, found ${elements.length} in ${funcName} at ${first.line}`
    );
  }
  if (first.token.content === "memory.size") {
    return ["i32"];
  }
  return [];
}

function unaryValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List,
  inputType: WatType,
  outputType: WatType
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;

  if (elements.length != 1) {
    throw new Error(
      `${first} should have 1 argument, found ${elements.length} in ${funcName} at ${first.line}`
    );
  }

  validateInstruction(funcName, context, elements[0], inputType);
  return outputType == "none" ? [] : [outputType];
}

function isComparison(instr: string) {
  return !!instr.match("eq|ne|[gl][et](_[us])?");
}

function binaryValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;

  if (elements.length != 2) {
    throw new Error(
      `${first} should have 2 arguments, found ${elements.length} in ${funcName} at ${first.line}`
    );
  }

  const parts = first.toString().split(".");
  if (parts.length !== 2 || !parts[0].match(kNumTypeReg)) {
    throw new Error(
      `${first} cannot be validated using the binary-op validator in ${funcName} at ${first.line}`
    );
  }
  const type: WatType = parts[0] as WatType;

  validateInstruction(funcName, context, elements[0], type);
  validateInstruction(funcName, context, elements[1], type);

  return [isComparison(parts[1]) ? "i32" : type];
}

function selectValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = assertAtom(elements.shift(), "select");

  if (elements.length != 3) {
    throw new Error(
      `${first} should have 3 arguments, found ${elements.length} in ${funcName} at ${first.line}`
    );
  }

  // TODO first and second instructions of select should have the expected output type
  validateInstruction(funcName, context, elements[0], "any");
  validateInstruction(funcName, context, elements[1], "any");
  validateInstruction(funcName, context, elements[2], "i32");

  return ["any"];
}

function blockValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  if (context.blocks.some((el) => el === name.token.content)) {
    throw new Error(
      `${first} has duplicate label ${name} in ${funcName} at ${name.line}`
    );
  }

  const blockContext: ValidatorContext = {
    ...context,
    blocks: [...context.blocks, name.token.content],
  };

  validateInstructions(funcName, blockContext, elements);

  return [];
}

function brifValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  if (!context.blocks.some((el) => el === name.token.content)) {
    throw new Error(
      `${first} has unknown label ${name} in ${funcName} at ${name.line}`
    );
  }

  if (elements.length != 1) {
    throw new Error(
      `${first} should have 1 argument, found ${elements.length} in ${funcName} at ${first.line}`
    );
  }

  validateInstruction(funcName, context, elements[0], "i32");
  return [];
}

function brValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = elements.shift() as Atom;
  const name = assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  if (!context.blocks.some((el) => el === name.token.content)) {
    throw new Error(
      `${first} has unknown label ${name} in ${funcName} at ${name.line}`
    );
  }

  if (elements.length) {
    throw new Error(
      `${first} should have no arguments, found ${elements.length} in ${funcName} at ${first.line}`
    );
  }
  return [];
}

function typeValidator(
  funcName: Atom,
  context: ValidatorContext,
  list: List
): WatType[] {
  const elements = Array.from(list.getSignificantElements());
  const first = assertAtom(elements.shift(), "type");
  assertAtomMatch(elements.shift(), kIdentifierReg, "identifier");

  if (elements.length) {
    throw new Error(
      `${first} should have no arguments, found ${elements.length} in ${funcName} at ${first.line}`
    );
  }
  return ["any"];
}
