import { expect } from "chai";
import "mocha";

import {
  checkForLeaks,
  commonExportsFromInstance,
  CommonTestExports,
  createString,
  IoEvent,
  IoTest,
  loadWasm,
} from "./common";

interface TestExports extends CommonTestExports {
  memory: WebAssembly.Memory;
  gHeap: () => number;
  gTrue: () => number;
  gFalse: () => number;
  gNil: () => number;
  mallocInit: () => void;
  runtimeInit: () => void;
  runtimeCleanup: () => void;
  stringToNumberImpl: (str: number, radix: number) => number;
  shortStrEq: (str: number, shortStr: number, shortStrLen: number) => number;
  atom: (token: number) => number;
  stringToDatum: (str: number) => number;
  read: () => number;
  print: (ptr: number) => void;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    ...commonExportsFromInstance(instance),
    gTrue: () => (instance.exports.gTrue as WebAssembly.Global).value as number,
    gFalse: () =>
      (instance.exports.gFalse as WebAssembly.Global).value as number,
    gNil: () => (instance.exports.gNil as WebAssembly.Global).value as number,
    mallocInit: instance.exports.mallocInit as () => void,
    mallocFree: instance.exports.mallocFree as (ptr: number) => void,
    runtimeInit: instance.exports.runtimeInit as () => void,
    runtimeCleanup: instance.exports.runtimeCleanup as () => void,
    stringToNumberImpl: instance.exports.stringToNumberImpl as (str: number, radix: number) => number,
    shortStrEq: instance.exports.shortStrEq as (
      str: number,
      shortStr: number,
      shortStrLen: number
    ) => number,
    atom: instance.exports.atom as (token: number) => number,
    stringToDatum: instance.exports.stringToDatum as (ptr: number) => number,
    read: instance.exports.read as () => number,
    print: instance.exports.print as (ptr: number) => void,
  };
}

describe("print wasm", () => {
  const io = new IoTest();
  const wasm = loadWasm(io.module);
  let exports: TestExports;

  const written: string[] = [];
  const writer = (event: IoEvent) => {
    if (event.data) {
      written.push(event.data);
    }
    return true;
  };

  const resetWriter = () => {
    written.splice(0, written.length);
  };

  before(async () => {
    const instance = await wasm;
    exports = exportsFromInstance(instance);
    io.exports = exports;
    exports.mallocInit();
    exports.runtimeInit();
    io.addEventListener("write", writer);
  });

  beforeEach(() => {
    resetWriter();
  });

  after(() => {
    io.removeEventListener("write", writer);
    exports.runtimeCleanup();
    checkForLeaks(exports);
  });

  it("prints nil as '()'", () => {
    exports.print(exports.gNil());
    expect(written.join("")).to.equal("()");
  });

  it("prints booleans as #t and #f", () => {
    exports.print(exports.gTrue());
    expect(written.join("")).to.equal("#t");
    resetWriter();

    exports.print(exports.gFalse());
    expect(written.join("")).to.equal("#f");
  });

  it("prints 32-bit integers correctly", () => {
    const kNumbers = [0, 1, -1, 0x7fffffff];

    for (const num of kNumbers) {
      const ptr = exports.heapAlloc(exports.gHeap(), 4, num, num < 0 ? -1 : 0);
      exports.print(ptr);
      expect(written.join("")).to.equal(num.toString());

      resetWriter();
    }
  });

  it("prints 64-bit integers correctly", () => {
    const kNumbers = [
      0n,
      1n,
      -1n,
      0x7fff_ffffn,
      0x8000_0000n,
      0x1_0000_0000n,
      123456789012345n,
      -67890123456789n,
    ];

    const bigint = new BigInt64Array(1);
    const words = new Int32Array(bigint.buffer);

    for (const num of kNumbers) {
      bigint[0] = num;

      const ptr = exports.heapAlloc(exports.gHeap(), 4, words[0], words[1]);
      exports.print(ptr);
      expect(written.join("")).to.equal(num.toString());

      resetWriter();
    }
  });

  it("prints dotted cons cell correctly", () => {
    const cons = exports.heapAlloc(
      exports.gHeap(),
      3,
      exports.heapAlloc(exports.gHeap(), 4, 1, 0),
      exports.heapAlloc(exports.gHeap(), 4, 2, 0)
    );

    exports.print(cons);
    expect(written.join("")).to.equal("(1 . 2)");
  });

  it("prints well formed list correctly", () => {
    const cons = exports.heapAlloc(
      exports.gHeap(),
      3,
      exports.heapAlloc(exports.gHeap(), 6, createString(exports, "+"), 0),
      exports.heapAlloc(
        exports.gHeap(),
        3,
        exports.heapAlloc(exports.gHeap(), 4, 2, 0),
        exports.heapAlloc(
          exports.gHeap(),
          3,
          exports.heapAlloc(exports.gHeap(), 4, 3, 0),
          exports.gNil()
        )
      )
    );

    exports.print(cons);
    expect(written.join("")).to.equal("(+ 2 3)");
  });
});
