import { expect } from "chai";
import { read } from "fs";
import "mocha";

import {
  checkForLeaks,
  checkMemory,
  commonExportsFromInstance,
  CommonTestExports,
  createHeapSymbol,
  createString,
  FileTest,
  getString,
  IoEvent,
  IoTest,
  loadWasm,
  ProcessTest,
  TestUnicode as UnicodeTest,
} from "./common";

interface TestExports extends CommonTestExports {
  memory: WebAssembly.Memory;
  gHeap: () => number;
  gTrue: () => number;
  gFalse: () => number;
  environmentInit: (heap: number, outer: number) => number;
  stringToNumberImpl: (str: number, radius: number) => number;
  shortStrEq: (str: number, shortStr: number, shortStrLen: number) => number;
  atom: (token: number) => number;
  stringToDatum: (str: number) => number;
  read: () => number;
  eval: (env: number, expr: number) => number;
  registerBuiltins: (heap: number, env: number) => void;
  print: (ptr: number) => void;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    ...commonExportsFromInstance(instance),
    memory: instance.exports.memory as WebAssembly.Memory,
    gHeap: () => (instance.exports.gHeap as WebAssembly.Global).value as number,
    gTrue: () => (instance.exports.gTrue as WebAssembly.Global).value as number,
    gFalse: () =>
      (instance.exports.gFalse as WebAssembly.Global).value as number,
    environmentInit: instance.exports.environmentInit as (
      heap: number,
      outer: number
    ) => number,
    stringToNumberImpl: instance.exports.stringToNumberImpl as (
      str: number,
      radix: number
    ) => number,
    shortStrEq: instance.exports.shortStrEq as (
      str: number,
      shortStr: number,
      shortStrLen: number
    ) => number,
    atom: instance.exports.atom as (token: number) => number,
    stringToDatum: instance.exports.stringToDatum as (ptr: number) => number,
    read: instance.exports.read as () => number,
    eval: instance.exports.eval as (env: number, expr: number) => number,
    registerBuiltins: instance.exports.registerBuiltins as (
      heap: number,
      env: number
    ) => void,
    print: instance.exports.print as (ptr: number) => void,
  };
}

describe("scheme", () => {
  const io = new IoTest();
  const file = new FileTest();
  const unicode = new UnicodeTest();
  const process = new ProcessTest();
  const wasm = loadWasm({
    io: io.module,
    file: file.module,
    unicode: unicode.module,
    process: process.module,
  });
  let exports: TestExports;
  const written: string[] = [];
  const writeHandler = (evt: IoEvent) => {
    if (evt.data) {
      written.push(evt.data);
    }
    return true;
  };

  before(async () => {
    const instance = await wasm;
    exports = exportsFromInstance(instance);
    io.exports = exports;
    io.addEventListener("write", writeHandler);
    file.exports = exports;
    unicode.exports = exports;
    process.exports = exports;
    await unicode.loadUnicodeBlocks();
  });

  beforeEach(() => {
    exports.mallocInit();
    exports.runtimeInit();
  });

  after(() => {
    io.removeEventListener("write", writeHandler);
  });

  afterEach(() => {
    if (written.length) {
      console.log(written.splice(0, written.length).join(""));
    }
    exports.runtimeCleanup();
    checkForLeaks(exports);
  });

  const testFile = async (filename: string) => {
    const inputs = [`(include \"prelude.scm\" \"${filename}\")`];
    const readHandler = (evt: IoEvent) => {
      evt.data = inputs.shift();
      return false;
    };
    io.addEventListener("read", readHandler);

    const env = exports.environmentInit(exports.gHeap(), 0);
    exports.registerBuiltins(exports.gHeap(), env);

    let expr = exports.read();

    try {
      while (true) {
        const result = exports.eval(env, expr);
        if (file.isImportPromise(result)) {
          expr = await file.addImportPromise(result);
        } else {
          exports.print(result);
          break;
        }
      }
    } catch (err) {
      console.error(err);
    }

    const output = written.join("");
    written.splice(0, written.length);

    io.removeEventListener("read", readHandler);

    const passing = output.match(/(\d+) passing/);
    const failing = output.match(/(\d+) failing/);
    if (passing && !failing) {
      return true;
    }
    console.log(output);
    checkMemory(exports);

    throw new Error(`scheme tests in ${filename} failed`);
  };

  it("test/boolean.spec.scm", async () => {
    await testFile("test/boolean.spec.scm");
  });

  it("test/bytevector.spec.scm", async () => {
    await testFile("test/bytevector.spec.scm");
  });

  it("test/case-lambda.spec.scm", async () => {
    await testFile("test/case-lambda.spec.scm");
  });

  it("test/char.spec.scm", async () => {
    await testFile("test/char.spec.scm");
  });

  it("test/complex.spec.scm", async () => {
    await testFile("test/complex.spec.scm");
  });

  it("test/conditionals.spec.scm", async () => {
    await testFile("test/conditionals.spec.scm");
  });

  it("test/control.spec.scm", async () => {
    await testFile("test/control.spec.scm");
  });

  it("test/cxr.spec.scm", async () => {
    await testFile("test/cxr.spec.scm");
  });

  it("test/equivalence.spec.scm", async () => {
    await testFile("test/equivalence.spec.scm");
  });

  it("test/exceptions.spec.scm", async () => {
    await testFile("test/exceptions.spec.scm");
  });

  it("test/include-ci.spec.scm", async () => {
    await testFile("test/include-ci.spec.scm");
  });

  it("test/lazy.spec.scm", async () => {
    await testFile("test/lazy.spec.scm");
  });

  it("test/number.spec.scm", async () => {
    await testFile("test/number.spec.scm");
  });

  it("test/pair.spec.scm", async () => {
    await testFile("test/pair.spec.scm");
  });

  it("test/process.spec.scm", async () => {
    await testFile("test/process.spec.scm");
  });

  it("test/sequence.spec.scm", async () => {
    await testFile("test/sequence.spec.scm");
  });

  it("test/string.spec.scm", async () => {
    await testFile("test/string.spec.scm");
  });

  it("test/symbol.spec.scm", async () => {
    await testFile("test/symbol.spec.scm");
  });

  it("test/time.spec.scm", async () => {
    await testFile("test/time.spec.scm");
  });

  it("test/trig.spec.scm", async () => {
    await testFile("test/trig.spec.scm");
  });

  it("test/vector.spec.scm", async () => {
    await testFile("test/vector.spec.scm");
  });
});
