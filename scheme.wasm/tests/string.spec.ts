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
  strDup: (ptr: number) => number;
  utf8FromCodePoint: (cp: number) => number;
  utf8CodePointSize: (cp: number) => number;
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
    strDup: instance.exports.strDup as (ptr: number) => number,
    utf8FromCodePoint: instance.exports.utf8FromCodePoint as (
      cp: number
    ) => number,
    utf8CodePointSize: instance.exports.utf8CodePointSize as (
      cp: number
    ) => number,
  };
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

  it("measures utf8 code point sizes", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    expect(exports.utf8CodePointSize(0x24)).to.equal(
      1,
      "utf8 encoding for '$' should be 1 byte long"
    );

    expect(exports.utf8CodePointSize(0xa2)).to.equal(
      2,
      "utf8 encoding for '¢' should be 2 bytes long"
    );

    expect(exports.utf8CodePointSize(0x20ac)).to.equal(
      3,
      "utf8 encoding for '€' should be 3 bytes long"
    );

    expect(exports.utf8CodePointSize(0x10348)).to.equal(
      4,
      "utf8 encoding for '𐍈' should be 4 bytes long"
    );

    const word = new Uint32Array(1);
    for (const invalid of [0x120000, 0xd800, 0xdfff, 0xdabc]) {
      expect(exports.utf8CodePointSize(invalid)).to.equal(
        3,
        `utf8 encoding for an invalid code point (${invalid.toString(
          16
        )}) should be 3 bytes long`
      );
    }
  });

  it("can check for string equality", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.malloc_init();

    const vectors: { a: string; b: string; res: number }[] = [
      { a: "aaaa", b: "aaaa", res: 1 },
      { a: "aaaa", b: "aaaA", res: 0 },
      { a: "aaaa", b: "aaa", res: 0 },
      { a: "aaaa", b: "aaaaa", res: 0 },
      { a: "aaaabbbbcc", b: "aaaabbbbcc", res: 1 },
      { a: "aaaabbbbcc", b: "aaaabbbbCc", res: 0 },
      { a: "aaaabbbbCc", b: "aaaabbbbcc", res: 0 },
      { a: "aaaabbbbcc", b: "aaaabbBbcc", res: 0 },
    ];

    for (const { a, b, res } of vectors) {
      const aPtr = createString(exports, a);
      const bPtr = createString(exports, b);

      expect(exports.strEq(aPtr, bPtr)).to.equal(
        res,
        `Expected ${JSON.stringify(a)} == ${JSON.stringify(b)} to be ${res}`
      );

      exports.malloc_free(aPtr);
      exports.malloc_free(bPtr);
    }
  });

  it("can duplicate strings", async() => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    const testString = "Hello, World!"

    const fromPtr = createString(exports, testString);
    const toPtr = exports.strDup(fromPtr);

    expect(toPtr).to.not.equal(fromPtr);
    expect(getString(exports, toPtr)).to.equal(testString);

    exports.malloc_free(fromPtr);
    exports.malloc_free(toPtr);
  });
});
