import { expect } from "chai";
import * as crypto from "crypto";
import "mocha";
import {
  checkForLeaks,
  checkMemory,
  commonExportsFromInstance,
  CommonTestExports,
  createString,
  loadWasm,
} from "./common";

interface MpTestExports extends CommonTestExports {
  mp10Pow: (pow: number) => number;
  mpAdd: (a: number, b: number) => number;
  mpSub: (a: number, b: number) => number;
  mpStringToMp: (str: number, strLen: number, base: number) => number;
  mpCleanup: () => void;
}

function createExportsFromInstance(
  instance: WebAssembly.Instance
): MpTestExports {
  return {
    ...commonExportsFromInstance(instance),
    mp10Pow: instance.exports.mp10Pow as (pow: number) => number,
    mpAdd: instance.exports.mpAdd as (a: number, b: number) => number,
    mpSub: instance.exports.mpSub as (a: number, b: number) => number,
    mpStringToMp: instance.exports.mpStringToMp as (
      str: number,
      strLen: number,
      base: number
    ) => number,
    mpCleanup: instance.exports.mpCleanup as () => void,
  };
}

describe("mp wasm", () => {
  const wasm = loadWasm();
  let exports: MpTestExports;

  before(async () => {
    const instance = await wasm;
    exports = createExportsFromInstance(instance);
    exports.mallocInit();
  });

  after(() => {
    exports.mpCleanup();
    checkForLeaks(exports);
  });

  afterEach(() => {
    checkMemory(exports);
  });

  const ptrToBigInt = (ptr: number) => {
    const words = new Uint32Array(exports.memory.buffer);
    const widx = ptr / 4;
    const len = words[widx] & 0x7fff_ffff;
    const sign = words[widx] & 0x8000_0000;
    let accum = 0n;
    for (let didx = 1; didx <= len; didx++) {
      accum *= 0x1_0000_0000n;
      accum |= BigInt(words[didx + widx]);
    }
    return sign === 0 ? accum : -accum;
  };

  const bigIntToPtr = (num: bigint) => {
    const words: number[] = [];
    const neg = num < 0n ? true : false;
    if (num < 0) {
      num = -num;
    }

    while (num !== 0n) {
      words.unshift(Number(BigInt.asUintN(32, num)));
      num >>= 32n;
    }

    const ptr = exports.malloc((words.length + 1) * 4);
    const view = new Uint32Array(exports.memory.buffer);
    const widx = ptr / 4;
    view[widx] = words.length | (neg ? 0x8000_0000 : 0x0000_0000);
    words.forEach((el, i) => (view[widx + i + 1] = el));

    return ptr;
  };

  const randomBigInt = (bits: number, maybeSigned?: boolean) => {
    const len = (bits + 0x3f) >> 6;
    const arr = new BigUint64Array(len);
    crypto.randomFillSync(arr);

    const val = arr.reduce((acc, el) => (acc << 64n) | el);
    return maybeSigned && Math.random() >= 0.5 ? -val : val;
  };

  it("generates small powers of 10 correctly", () => {
    let pow10 = 1n;
    for (let i = 0; i <= 19; i++) {
      const val = exports.mp10Pow(i);
      const bigVal = ptrToBigInt(val);
      expect(bigVal).to.equal(pow10, `computing 10^${i}`);
      pow10 *= 10n;
    }
  });

  it("generates larger powers of 10 correctly", () => {
    let pow10 = 100_000_000_000_000_000_000n;

    for (let i = 20; i <= 40; i += 5) {
      const val = exports.mp10Pow(i);
      const bigVal = ptrToBigInt(val);
      expect(bigVal).to.equal(pow10, `computing 10^${i}`);
      pow10 *= 100_000n;
      checkMemory(exports);
    }
  });

  it("adds positive numbers", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128);
      const b = randomBigInt(128);
      const a_ptr = bigIntToPtr(a);
      const b_ptr = bigIntToPtr(b);

      const val = exports.mpAdd(a_ptr, b_ptr);
      const bigVal = ptrToBigInt(val);
      expect(bigVal).to.equal(a + b, `computing ${a} + ${b}`);

      exports.mallocFree(a_ptr);
      exports.mallocFree(b_ptr);
      exports.mallocFree(val);
    }
  });

  it("subtracts numbers", () => {
    const a = 267197213709374958260730313680667588572n;
    const b = 44926186424055215012510765029051707353n;
    const c = 222271027285319743248219548651615881219n;

    const a_ptr = bigIntToPtr(a);
    const b_ptr = bigIntToPtr(b);

    const val = exports.mpSub(a_ptr, b_ptr);
    const bigVal = ptrToBigInt(val);
    expect(bigVal).to.equal(a - b, `computing ${a} - ${b}`);

    exports.mallocFree(a_ptr);
    exports.mallocFree(b_ptr);
    exports.mallocFree(val);
  });

  it("adds positive and negative numbers", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128, true);
      const b = randomBigInt(128, true);
      const a_ptr = bigIntToPtr(a);
      const b_ptr = bigIntToPtr(b);

      const val = exports.mpAdd(a_ptr, b_ptr);
      const bigVal = ptrToBigInt(val);
      expect(bigVal).to.equal(a + b, `computing ${a} + ${b}`);

      exports.mallocFree(a_ptr);
      exports.mallocFree(b_ptr);
      exports.mallocFree(val);
    }
  });

  it("converts base-10 strings to numbers", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128, true);
      const a_str = a.toString();
      const a_str_ptr = createString(exports, a_str);

      const val = exports.mpStringToMp(a_str_ptr + 4, a_str.length, 10);
      const bigVal = ptrToBigInt(val);
      expect(bigVal).to.equal(a);

      exports.mallocFree(a_str_ptr);
      exports.mallocFree(val);
    }
  });

  it("converts base-16 strings to numbers", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128, true);
      const a_str = a.toString(16);
      const a_str_ptr = createString(exports, a_str);

      const val = exports.mpStringToMp(a_str_ptr + 4, a_str.length, 16);
      const bigVal = ptrToBigInt(val);
      expect(bigVal).to.equal(
        a,
        `Converting ${a_str}, got ${bigVal.toString(16)}`
      );

      exports.mallocFree(a_str_ptr);
      exports.mallocFree(val);
    }
  });

  it("converts base-8 strings to numbers", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128, true);
      const a_str = a.toString(8);
      const a_str_ptr = createString(exports, a_str);

      const val = exports.mpStringToMp(a_str_ptr + 4, a_str.length, 8);
      const bigVal = ptrToBigInt(val);
      expect(bigVal).to.equal(
        a,
        `Converting ${a_str}, got ${bigVal.toString(8)}`
      );

      exports.mallocFree(a_str_ptr);
      exports.mallocFree(val);
    }
  });

  it("converts base-2 strings to numbers", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128, true);
      const a_str = a.toString(2);
      const a_str_ptr = createString(exports, a_str);

      const val = exports.mpStringToMp(a_str_ptr + 4, a_str.length, 2);
      const bigVal = ptrToBigInt(val);
      expect(bigVal).to.equal(
        a,
        `Converting ${a_str}, got ${bigVal.toString(2)}`
      );

      exports.mallocFree(a_str_ptr);
      exports.mallocFree(val);
    }
  });
});
