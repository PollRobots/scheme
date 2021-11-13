import { expect } from "chai";
import "mocha";

import {
  checkForLeaks,
  createHeapSymbol,
  createString,
  IoEvent,
  IoTest,
  loadWasm,
} from "./common";

interface TestExports {
  memory: WebAssembly.Memory;
  gHeap: () => number;
  gTrue: () => number;
  gFalse: () => number;
  mallocInit: () => void;
  mallocFree: (ptr: number) => void;
  strFrom32: (len: number, val: number) => number;
  strFrom64: (len: number, val: bigint) => number;
  strFrom128: (len: number, val1: bigint, val2: bigint) => number;
  heapAlloc: (
    heap: number,
    type: number,
    data1: number,
    data2: number
  ) => number;
  runtimeInit: () => void;
  runtimeCleanup: () => void;
  environmentInit: (heap: number, outer: number) => number;
  stringToNumber: (str: number) => number;
  shortStrEq: (str: number, shortStr: number, shortStrLen: number) => number;
  atom: (token: number) => number;
  stringToDatum: (str: number) => number;
  read: () => number;
  eval: (env: number, expr: number) => number;
  registerBuiltins: (heap: number, env: number) => void;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    memory: instance.exports.memory as WebAssembly.Memory,
    gHeap: () => (instance.exports.gHeap as WebAssembly.Global).value as number,
    gTrue: () => (instance.exports.gTrue as WebAssembly.Global).value as number,
    gFalse: () =>
      (instance.exports.gFalse as WebAssembly.Global).value as number,
    mallocInit: instance.exports.mallocInit as () => void,
    mallocFree: instance.exports.mallocFree as (ptr: number) => void,
    strFrom32: instance.exports.strFrom32 as (
      len: number,
      val: number
    ) => number,
    strFrom64: instance.exports.strFrom64 as (
      len: number,
      val: bigint
    ) => number,
    strFrom128: instance.exports.strFrom128 as (
      len: number,
      val1: bigint,
      val2: bigint
    ) => number,
    heapAlloc: instance.exports.heapAlloc as (
      heap: number,
      type: number,
      data1: number,
      data2: number
    ) => number,
    runtimeInit: instance.exports.runtimeInit as () => void,
    runtimeCleanup: instance.exports.runtimeCleanup as () => void,
    environmentInit: instance.exports.environmentInit as (
      heap: number,
      outer: number
    ) => number,
    stringToNumber: instance.exports.stringToNumber as (str: number) => number,
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
  };
}

describe("runtime wasm", () => {
  const io = new IoTest();
  const wasm = loadWasm(io.module);
  let exports: TestExports;

  before(async () => {
    const instance = await wasm;
    exports = exportsFromInstance(instance);
    io.exports = exports;
    exports.mallocInit();
    exports.runtimeInit();
  });

  after(() => {
    exports.runtimeCleanup();
    checkForLeaks(exports);
  });

  it("converts strings to numbers", () => {
    const kTestNumbers: { str: string; ex: number }[] = [
      { str: "1", ex: 1 },
      { str: "#b1", ex: 1 },
      { str: "#o1", ex: 1 },
      { str: "#d1", ex: 1 },
      { str: "#x1", ex: 1 },
      { str: "#b10010010110", ex: 0b10010010110 },
      { str: "#o1234", ex: 0o1234 },
      { str: "#d1234", ex: 1234 },
      { str: "#x1234", ex: 0x1234 },
      { str: "#xabef", ex: 0xabef },
      { str: "#xABEF", ex: 0xabef },
      { str: "-42", ex: -42 },
      { str: "#d-42", ex: -42 },
      { str: "#x-42", ex: -0x42 },
    ];

    for (const { str, ex } of kTestNumbers) {
      const strPtr = createString(exports, str);
      const strHeapPtr = exports.heapAlloc(exports.gHeap(), 7, strPtr, 0);
      const numHeapPtr = exports.stringToNumber(strHeapPtr);

      const words = new Int32Array(
        exports.memory.buffer.slice(numHeapPtr, numHeapPtr + 12)
      );
      expect(words[0]).to.equal(
        4,
        `string->number(${str}) return value should be an integer`
      );
      expect(words[1]).to.equal(
        ex,
        `this should be the numeric value of '${str}'`
      );
      if (ex >= 0) {
        expect(words[2]).to.equal(
          0,
          `string->number(${str}) This should be the sign extension of a positive 32 bit integer`
        );
      } else {
        expect(words[2]).to.equal(
          -1,
          `string->number(${str}) This should be the sign extension of a negative 32 bit integer`
        );
      }
    }

    const kTestInvalidNumbers: string[] = [
      "1#x",
      "1_200",
      "1,200",
      "--2",
      "#i43",
      "#e8",
      "#b107",
      "#o108",
      "#x1G",
      "#d1e",
    ];

    for (const str of kTestInvalidNumbers) {
      const strPtr = createString(exports, str);
      const strHeapPtr = exports.heapAlloc(exports.gHeap(), 7, strPtr, 0);
      const numHeapPtr = exports.stringToNumber(strHeapPtr);
      const words = new Int32Array(
        exports.memory.buffer.slice(numHeapPtr, numHeapPtr + 12)
      );
      expect(words[0]).to.equal(
        2,
        `string->number(${str}) return value should be a boolean`
      );
      expect(words[1]).to.equal(
        0,
        `string->number(${str}) return value should be false`
      );
      expect(numHeapPtr).to.equal(
        exports.gFalse(),
        `should use the global false value`
      );
    }
  });

  it("can compare tiny strings", () => {
    const kTestStrings: {
      str: string;
      bytes: number;
      len: number;
      ex: boolean;
    }[] = [
      { str: "(", bytes: 0x28, len: 1, ex: true },
      { str: ")", bytes: 0x29, len: 1, ex: true },
      { str: "foo", bytes: 0x6f6f66, len: 3, ex: true },
      { str: "€", bytes: 0xac82e2, len: 3, ex: true },
      { str: "bar", bytes: 0x6162, len: 2, ex: false },
      { str: "bar", bytes: 0x726162, len: 3, ex: true },
      { str: "bar", bytes: 0x726163, len: 3, ex: false },
    ];

    for (const { str, bytes, len, ex } of kTestStrings) {
      const strPtr = createString(exports, str);
      expect(exports.shortStrEq(strPtr, bytes, len)).to.equal(
        ex ? 1 : 0,
        `short-str-eq(${JSON.stringify(str)}, 0x${bytes.toString(
          16
        )}, ${len}) should return ${ex}`
      );
      exports.mallocFree(strPtr);
    }

    const longer = createString(exports, "abcdE");
    expect(() => exports.shortStrEq(longer, 0x64636261, 5)).to.throw(
      "unreachable"
    );
    exports.mallocFree(longer);
  });

  const makeHeapString = (str: string): number => {
    const strPtr = createString(exports, str);
    return exports.heapAlloc(exports.gHeap(), 0x7, strPtr, 0);
  };

  it("will make a 'true' atom from #t", () => {
    let heapItem = makeHeapString("#t");
    let res = exports.atom(heapItem);
    let words = new Uint32Array(exports.memory.buffer.slice(res, res + 12));
    expect(words[0]).to.equal(2, "#t should make a boolean");
    expect(words[1]).to.equal(1, "#t should make a true boolean");
    expect(res).to.equal(exports.gTrue());
  });

  it("will make a 'false' atom from #f", () => {
    const heapItem = makeHeapString("#f");
    const res = exports.atom(heapItem);
    const words = new Uint32Array(exports.memory.buffer.slice(res, res + 12));
    expect(words[0]).to.equal(2, "#f should make a boolean");
    expect(words[1]).to.equal(0, "#f should make a false boolean");
    expect(res).to.equal(exports.gFalse());
  });

  it("will make an integer atom from #xFF", () => {
    const heapItem = makeHeapString("#xFF");
    const res = exports.atom(heapItem);
    const words = new Uint32Array(exports.memory.buffer.slice(res, res + 12));
    expect(words[0]).to.equal(4, "#xFF should make a number");
    expect(words[1]).to.equal(0xff, "#xFF be 0xFF");
    expect(words[2]).to.equal(0), "#xFF should be 0 in the upper word";
  });

  it("will make a integer atom from 42", () => {
    const heapItem = makeHeapString("42");
    const res = exports.atom(heapItem);
    const words = new Uint32Array(exports.memory.buffer.slice(res, res + 12));
    expect(words[0]).to.equal(4, "42 should make a number");
    expect(words[1]).to.equal(42);
    expect(words[2]).to.equal(0), "42 should be 0 in the upper word";
  });

  it('will make a symbol atom from "foo"', () => {
    const heapItem = makeHeapString("foo");
    let words = new Uint32Array(
      exports.memory.buffer.slice(heapItem, heapItem + 12)
    );
    const fooStr = words[1];
    const res = exports.atom(heapItem);
    words = new Uint32Array(exports.memory.buffer.slice(res, res + 12));
    expect(words[0]).to.equal(6, "foo should make a symbol");
    expect(words[1]).to.equal(
      fooStr,
      "the symbol should point to the original string"
    );
    expect(words[2]).to.equal(0), "a symbol should have 0 in the upper word";
  });

  it("string->datum will convert '42' to an integer item", () => {
    const input = createString(exports, "42");
    const datum = exports.stringToDatum(input);
    const words = new Uint32Array(
      exports.memory.buffer.slice(datum, datum + 12)
    );
    expect(words[0]).to.equal(4, "42 should make a number");
    expect(words[1]).to.equal(42);
    expect(words[2]).to.equal(0), "42 should be 0 in the upper word";
  });

  it("string->datum will convert '()' to a nil item", () => {
    const readHandler = (evt: IoEvent) => {
      evt.data = ")";
      return false;
    };
    io.addEventListener("read", readHandler);
    const input = createString(exports, "(");
    const datum = exports.stringToDatum(input);
    const words = new Uint32Array(
      exports.memory.buffer.slice(datum, datum + 12)
    );
    expect(words[0]).to.equal(1, "() should make a nil datum");
    io.removeEventListener("read", readHandler);
  });

  it("string->datum will convert '(1 2 3)' to a three item list", () => {
    const tokens = ["1 2 3 )"];

    const readHandler = (evt: IoEvent) => {
      evt.data = tokens.shift();
      return false;
    };
    io.addEventListener("read", readHandler);
    const input = createString(exports, "(");
    const datum = exports.stringToDatum(input);
    let words = new Uint32Array(exports.memory.buffer.slice(datum, datum + 12));

    const items: number[] = [];
    while (true) {
      expect(words[0]).to.equal(3, "should be a cons cell");
      const car = words[1];
      const carWords = new Uint32Array(
        exports.memory.buffer.slice(car, car + 12)
      );
      expect(carWords[0]).to.equal(4);
      items.push(carWords[1]);

      const cdr = words[2];
      words = new Uint32Array(exports.memory.buffer.slice(cdr, cdr + 12));
      if (words[0] == 1) {
        // nil cdr means end of list
        break;
      }
    }

    expect(items).to.have.ordered.members([1, 2, 3]);

    io.removeEventListener("read", readHandler);
  });

  it("string->datum will convert '(1 . 3)' to a cons cell", () => {
    const tokens = ["1 . 3 )"];

    const readHandler = (evt: IoEvent) => {
      evt.data = tokens.shift();
      return false;
    };
    io.addEventListener("read", readHandler);
    const input = createString(exports, "(");
    const datum = exports.stringToDatum(input);
    let words = new Uint32Array(exports.memory.buffer.slice(datum, datum + 12));

    expect(words[0]).to.equal(3, "should be a cons cell");
    const car = words[1];
    const carWords = new Uint32Array(
      exports.memory.buffer.slice(car, car + 12)
    );
    expect(carWords[0]).to.equal(4, "first item should be an integer");
    expect(carWords[1]).to.equal(1, "first item should be 1");

    const cdr = words[2];
    const cdrWords = new Uint32Array(
      exports.memory.buffer.slice(cdr, cdr + 12)
    );
    expect(cdrWords[0]).to.equal(4, "second item should be an integer");
    expect(cdrWords[1]).to.equal(3, "second item should be 3");

    io.removeEventListener("read", readHandler);
  });

  it("read will read '(1 2 3)' as a three item list", () => {
    const tokens = ["(1 2 ", " 3 )"];

    const readHandler = (evt: IoEvent) => {
      evt.data = tokens.shift();
      return false;
    };
    io.addEventListener("read", readHandler);
    const datum = exports.read();
    let words = new Uint32Array(exports.memory.buffer.slice(datum, datum + 12));

    const items: number[] = [];
    while (true) {
      expect(words[0]).to.equal(3, "should be a cons cell");
      const car = words[1];
      const carWords = new Uint32Array(
        exports.memory.buffer.slice(car, car + 12)
      );
      expect(carWords[0]).to.equal(4);
      items.push(carWords[1]);

      const cdr = words[2];
      words = new Uint32Array(exports.memory.buffer.slice(cdr, cdr + 12));
      if (words[0] == 1) {
        // nil cdr means end of list
        break;
      }
    }

    expect(items).to.have.ordered.members([1, 2, 3]);

    io.removeEventListener("read", readHandler);
  });

  it("evals numbers to themselves", () => {
    const env = exports.environmentInit(exports.gHeap(), 0);
    exports.registerBuiltins(exports.gHeap(), env);

    const datum = exports.heapAlloc(exports.gHeap(), 4, 1234, 0);
    const result = exports.eval(env, datum);
    expect(exports.eval(env, datum)).to.equal(datum, "numbers should eval to themselves");
  })

  it("evals symbols to builtins via environment lookup", () => {
    const env = exports.environmentInit(exports.gHeap(), 0);
    exports.registerBuiltins(exports.gHeap(), env);

    const datum = createHeapSymbol(exports, '+');
    const result = exports.eval(env, datum);
    const words = new Uint32Array(exports.memory.buffer.slice(result, result + 12));
    expect(words[0]).to.equal(11, "expect the result to be a builtin (type 11)")
  })

  it("can eval simple expressions", () => {
    const tokens = ["(+ 1 (* 2 3))"];
    const readHandler = (evt: IoEvent) => {
      evt.data = tokens.shift();
      return false;
    };
    io.addEventListener("read", readHandler);

    const env = exports.environmentInit(exports.gHeap(), 0);
    exports.registerBuiltins(exports.gHeap(), env);

    const datum = exports.read();

    const result = exports.eval(env, datum);
    const words = new Uint32Array(
      exports.memory.buffer.slice(result, result + 12)
    );
    expect(words[0]).to.equal(4, "result type should be an i64");
    expect(words[1]).to.equal(7, "1 + 2 * 3 = 7");
    expect(words[2]).to.equal(0);

    io.removeEventListener("read", readHandler);
  });
});
