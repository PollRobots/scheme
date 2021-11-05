import { expect } from "chai";
import "mocha";

import { createString, getString, loadWasm } from "./common";

interface TestExports {
  memory: WebAssembly.Memory;
  malloc_init: () => void;
  malloc_free: (ptr: number) => void;
  strFrom32: (len: number, val: number) => number;
  strFrom64: (len: number, val: bigint) => number;
  strFrom128: (len: number, val1: bigint, val2: bigint) => number;
  strByteLen: (ptr: number) => number;
  strCodePointLen: (ptr: number) => number;
  strCodePointAt: (ptr: number, at: number) => number;
  strIsValid: (ptr: number) => number;
  strEq: (a: number, b: number) => number;
  utf8FromCodePoint: (cp: number) => number;
  utf8CodePointSize: (cp: number) => number;
  hashtableInit: (capacity: number) => number;
  hashtableAdd: (ptr: number, key: number, value: number) => number;
  hashtableGet: (ptr: number, key: number) => number;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    memory: instance.exports.memory as WebAssembly.Memory,
    malloc_init: instance.exports.malloc_init as () => void,
    malloc_free: instance.exports.malloc_free as (ptr: number) => void,
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
    strByteLen: instance.exports.strByteLen as (ptr: number) => number,
    strCodePointLen: instance.exports.strCodePointLen as (
      ptr: number
    ) => number,
    strCodePointAt: instance.exports.strCodePointAt as (
      ptr: number,
      at: number
    ) => number,
    strIsValid: instance.exports.strIsValid as (ptr: number) => number,
    strEq: instance.exports.strEq as (a: number, b: number) => number,
    utf8FromCodePoint: instance.exports.utf8FromCodePoint as (
      cp: number
    ) => number,
    utf8CodePointSize: instance.exports.utf8CodePointSize as (
      cp: number
    ) => number,
    hashtableInit: instance.exports.hashtableInit as (
      capacity: number
    ) => number,
    hashtableAdd: instance.exports.hashtableAdd as (
      ptr: number,
      key: number,
      value: number
    ) => number,
    hashtableGet: instance.exports.hashtableGet as (
      ptr: number,
      key: number
    ) => number,
  };
}

describe("hashtable wasm", () => {
  const wasm = loadWasm();
  let exports: TestExports;

  before(async () => {
    const instance = await wasm;
    exports = exportsFromInstance(instance);
    exports.malloc_init();
  });

  it("Can allocate and initialize a hashtable", () => {
    const ptr = exports.hashtableInit(0);
    expect(ptr).to.be.greaterThanOrEqual(40);
    expect(ptr).to.be.lessThan(exports.memory.buffer.byteLength);

    const words = new Uint32Array(exports.memory.buffer);
    expect(words[ptr / 4]).to.equal(
      32,
      "Default initial capacity should be 32"
    );
    expect(words[(ptr + 4) / 4]).to.equal(0, "hashtable should be empty");
    exports.malloc_free(ptr);
  });

  it("Can store and retrieve data in a hashtable", () => {
    const words = new Uint32Array(exports.memory.buffer);
    const ptr = exports.hashtableInit(0);

    const testData: Record<string, number> = {
      a: 1,
      b: 2,
      c: 3,
      d: 4,
      e: 5,
    };

    const testStrings: Record<string, number> = {};

    let count = 0;
    for (const key of Object.keys(testData)) {
      const testString = createString(exports, key);
      const value = testData[key];
      testStrings[key] = testString;

      expect(
        exports.hashtableAdd(ptr, createString(exports, key), value)
      ).to.equal(ptr, "hashtable should not grow");
      count++;
      expect(words[(ptr + 4) / 4]).to.equal(count);
    }

    for (const key of Object.keys(testData)) {
      const res = exports.hashtableGet(ptr, testStrings[key]);
      expect(res).to.equal(
        testData[key],
        `fetching '${key}' as ${testStrings[key]}`
      );
    }

    const none = createString(exports, "none");
    expect(exports.hashtableGet(ptr, none)).to.equal(0);
    exports.malloc_free(none);

    exports.malloc_free(ptr);
    for (const key of Object.keys(testData)) {
      exports.malloc_free(testStrings[key]);
    }
  });

  it("can grow a hashtable when needed", () => {
    const words = new Uint32Array(exports.memory.buffer);
    const ptr = exports.hashtableInit(0);

    const strings: Record<number, number> = {};

    // the hashtable grows when it hits 75% capacity
    const limit = (3 * 32) / 4;

    for (let i = 0; i != limit; i++) {
      const str = createString(exports, i.toString(16).padStart(2, "0"));
      strings[i] = str;

      expect(exports.hashtableAdd(ptr, str, i + 1)).to.equal(
        ptr,
        "hashtable should not grow"
      );
    }

    const grow = createString(exports, "grow");
    const newPtr = exports.hashtableAdd(ptr, grow, 0xffff);
    expect(newPtr).to.not.equal(ptr, "hashtable should have grown");

    exports.malloc_free(grow);
    for (let i = 0; i < limit; i++) {
      exports.malloc_free(strings[i]);
    }
    exports.malloc_free(newPtr);
  });
});