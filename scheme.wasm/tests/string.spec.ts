import { expect } from "chai";
import { create } from "domain";
import "mocha";
import { loadWasm } from "./common";

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
  utf8FromCodePoint: (cp: number) => number;
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
    utf8FromCodePoint: instance.exports.utf8FromCodePoint as (
      cp: number
    ) => number,
  };
}

function createString(exports: TestExports, str: string): number {
  const array = new TextEncoder().encode(str);
  if (array.byteLength > 16) {
    throw new Error(
      `Cannot create strings with a byte length of greater than 16 '${str}' has a utf-8 byte length of ${array.byteLength}.`
    );
  }
  const extended = new Uint8Array(16);
  extended.set(array);
  if (array.byteLength <= 4) {
    const words = new Uint32Array(extended.buffer);
    return exports.strFrom32(array.byteLength, words[0]);
  }
  const big = new BigUint64Array(extended.buffer);

  if (array.byteLength <= 8) {
    return exports.strFrom64(array.byteLength, big[0]);
  } else {
    return exports.strFrom128(array.byteLength, big[0], big[1]);
  }
}

function getString(exports: TestExports, ptr: number): string {
  const view = new Uint8Array(exports.memory.buffer);
  const words = new Uint32Array(exports.memory.buffer);

  expect(ptr).to.be.greaterThanOrEqual(
    32,
    `String ptr 0x${ptr.toString(16).padStart(8, "0")} is out of bounds (32 – ${
      view.byteLength
    })`
  );
  expect(ptr).to.be.lessThanOrEqual(
    view.byteLength - 8,
    `String ptr 0x${ptr.toString(16).padStart(8, "0")} is out of bounds (32 – ${
      view.byteLength
    })`
  );
  expect(ptr & 0xfffffff8).to.equal(
    ptr,
    `String ptr 0x${ptr.toString(16).padStart(8, "0")} is badly aligned`
  );

  const len = words[ptr / 4];
  return new TextDecoder().decode(view.slice(ptr + 4, ptr + 4 + len));
}

describe("string wasm", () => {
  const wasm = loadWasm();

  it("allocates tiny strings", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    const view = new Uint8Array(exports.memory.buffer);
    const words = new Uint32Array(exports.memory.buffer);

    const ptr = exports.strFrom32(4, 0x44434241);
    expect(exports.strByteLen(ptr)).to.equal(4, "length should be 4");
    expect(getString(exports, ptr)).to.equal("ABCD");

    exports.malloc_free(ptr);
  });

  it("throws if tiny string is too long", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    expect(() => exports.strFrom32(5, 0)).throws("unreachable");
  });

  it("allocates short strings", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    const view = new Uint8Array(exports.memory.buffer);
    const words = new Uint32Array(exports.memory.buffer);

    const ptr = exports.strFrom64(8, 0x4847464544434241n);
    expect(exports.strByteLen(ptr)).to.equal(8, "length should be 8");
    expect(getString(exports, ptr)).to.equal("ABCDEFGH");

    exports.malloc_free(ptr);
  });

  it("throws if short string is too long", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    expect(() => exports.strFrom64(9, 0n)).throws("unreachable");
  });

  it("allocates medium strings", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    const view = new Uint8Array(exports.memory.buffer);
    const words = new Uint32Array(exports.memory.buffer);

    const ptr = exports.strFrom128(
      16,
      0x4847464544434241n,
      0x504f4e4d4c4b4a49n
    );
    expect(exports.strByteLen(ptr)).to.equal(16, "length should be 16");
    expect(getString(exports, ptr)).to.equal("ABCDEFGHIJKLMNOP");

    exports.malloc_free(ptr);
  });

  it("throws if medium string is too long", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    expect(() => exports.strFrom128(17, 0n, 0n)).throws("unreachable");
  });

  it("reports code point lengths", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    const ptr = createString(exports, "$¢€𐍈");
    expect(exports.strByteLen(ptr)).to.equal(10);
    expect(getString(exports, ptr)).to.equal("$¢€𐍈");
    expect(exports.strCodePointLen(ptr)).to.equal(4);

    exports.malloc_free(ptr);
  });

  it("fetches code points by index", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    const ptr = createString(exports, "$¢€𐍈");
    expect(exports.strByteLen(ptr)).to.equal(10);
    expect(getString(exports, ptr)).to.equal("$¢€𐍈");

    expect(String.fromCodePoint(exports.strCodePointAt(ptr, 0))).to.equal("$");
    expect(String.fromCodePoint(exports.strCodePointAt(ptr, 1))).to.equal("¢");
    expect(String.fromCodePoint(exports.strCodePointAt(ptr, 2))).to.equal("€");
    expect(String.fromCodePoint(exports.strCodePointAt(ptr, 3))).to.equal("𐍈");

    exports.malloc_free(ptr);
  });

  it("checks string validity", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    const ptr = createString(exports, "$¢€𐍈");
    expect(exports.strIsValid(ptr));
    exports.malloc_free(ptr);

    // negative cases
    const cases: [number, number][] = [
      [0x8f, 1], // misplaced continuation character
      [0xfe, 1], // invalid character
      [0xff, 1], // invalid character
      [0x8f8fbff5, 4], // invalid 4 byte character (too big)
    ];

    for (const [str, len] of cases) {
      const ptr = exports.strFrom32(len, str);
      expect(exports.strIsValid(ptr)).to.equal(
        0,
        `str: ${str.toString(16).padStart(8, "0")}, len: ${len}`
      );
      exports.malloc_free(ptr);
    }
  });

  const decodeUtf8Word = (word: number) => {
    const buffer = new Uint32Array([word]);
    const len =
      buffer[0] < 0x100
        ? 1
        : buffer[0] < 0x10000
        ? 2
        : buffer[0] < 0x1000000
        ? 3
        : 4;
    const bytes = new Uint8Array(buffer.buffer).slice(0, len);
    return new TextDecoder().decode(bytes);
  };

  it("converts from a code point to utf8", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    const dollar = exports.utf8FromCodePoint(0x24);
    expect(decodeUtf8Word(dollar)).to.equal("$");

    const cent = exports.utf8FromCodePoint(0xa2);
    expect(decodeUtf8Word(cent)).to.equal("¢");

    const euro = exports.utf8FromCodePoint(0x20ac);
    expect(decodeUtf8Word(euro)).to.equal("€");

    const hwair = exports.utf8FromCodePoint(0x10348);
    expect(decodeUtf8Word(hwair)).to.equal("𐍈");

    const word = new Uint32Array(1);
    for (const invalid of [0x120000, 0xd800, 0xdfff, 0xdabc]) {
      const replacement = exports.utf8FromCodePoint(invalid);
      word[0] = replacement;
      expect(decodeUtf8Word(replacement)).to.equal(
        "�",
        `encoding 0x${invalid.toString(16)}, got 0x${word[0].toString(16)}`
      );
    }
  });
});
