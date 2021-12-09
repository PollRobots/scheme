import { expect } from "chai";
import * as crypto from "crypto";
import "mocha";
import {
  checkForLeaks,
  checkMemory,
  commonExportsFromInstance,
  CommonTestExports,
  createString,
  getString,
  loadWasm,
} from "./common";

interface MpTestExports extends CommonTestExports {
  mp10Pow: (pow: number) => number;
  mpAdd: (a: number, b: number) => number;
  mpSub: (a: number, b: number) => number;
  mpDiv: (dividend: number, divisor: number) => bigint;
  mpShl: (a: number, shift: number) => number;
  mpShrIp: (a: number, shift: number) => void;
  mpEq: (a: number, b: number) => number;
  mpGt: (a: number, b: number) => number;
  mpGe: (a: number, b: number) => number;
  mpLt: (a: number, b: number) => number;
  mpLe: (a: number, b: number) => number;
  mpStringToMp: (str: number, strLen: number, base: number) => number;
  mpMpToString: (ptr: number, base: number) => number;
  mpIsZero: (pow: number) => number;
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
    mpDiv: instance.exports.mpDiv as (
      dividend: number,
      divisor: number
    ) => bigint,
    mpShl: instance.exports.mpShl as (a: number, shift: number) => number,
    mpShrIp: instance.exports.mpShrIp as (a: number, shift: number) => void,
    mpEq: instance.exports.mpEq as (a: number, b: number) => number,
    mpGt: instance.exports.mpGt as (a: number, b: number) => number,
    mpGe: instance.exports.mpGe as (a: number, b: number) => number,
    mpLt: instance.exports.mpLt as (a: number, b: number) => number,
    mpLe: instance.exports.mpLe as (a: number, b: number) => number,
    mpStringToMp: instance.exports.mpStringToMp as (
      str: number,
      strLen: number,
      base: number
    ) => number,
    mpMpToString: instance.exports.mpMpToString as (
      ptr: number,
      base: number
    ) => number,
    mpIsZero: instance.exports.mpIsZero as (pow: number) => number,
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

    let val = arr.reduce((acc, el) => (acc << 64n) | el);
    const shift = BigInt((len << 6) - bits);
    if (shift > 0n) {
      val = val >> shift;
    }
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

  it("converts numbers to base-10 strings", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128, true);
      const a_ptr = bigIntToPtr(a);

      const val = exports.mpMpToString(a_ptr, 10);
      expect(getString(exports, val)).to.equal(a.toString());

      exports.mallocFree(a_ptr);
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

  it("converts numbers to base-16 strings", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128, true);
      const a_ptr = bigIntToPtr(a);

      const val = exports.mpMpToString(a_ptr, 16);
      expect(getString(exports, val).toLowerCase()).to.equal(a.toString(16));

      exports.mallocFree(a_ptr);
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

  it("converts numbers to base-8 strings", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128, true);
      const a_ptr = bigIntToPtr(a);

      const val = exports.mpMpToString(a_ptr, 8);
      expect(getString(exports, val).toLowerCase()).to.equal(a.toString(8));

      exports.mallocFree(a_ptr);
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

  it("converts numbers to base-2 strings", () => {
    for (let i = 0; i < 20; i++) {
      const a = randomBigInt(128, true);
      const a_ptr = bigIntToPtr(a);

      const val = exports.mpMpToString(a_ptr, 2);
      expect(getString(exports, val).toLowerCase()).to.equal(a.toString(2));

      exports.mallocFree(a_ptr);
      exports.mallocFree(val);
    }
  });

  it("can check if a number is zero", () => {
    const zero = bigIntToPtr(0n);
    const one = bigIntToPtr(1n);
    const minus_one = bigIntToPtr(-1n);
    const rand = bigIntToPtr(randomBigInt(128));

    expect(exports.mpIsZero(zero)).to.equal(
      1,
      `(mp-zero? ${zero}) should be 1`
    );
    expect(exports.mpIsZero(one)).to.equal(0, `(mp-zero? ${one}) should be 0`);
    expect(exports.mpIsZero(minus_one)).to.equal(
      0,
      `(mp-zero? ${minus_one}) should be 0`
    );
    expect(exports.mpIsZero(rand)).to.equal(
      0,
      `(mp-zero? ${rand}) should be 0`
    );

    exports.mallocFree(zero);
    exports.mallocFree(one);
    exports.mallocFree(minus_one);
    exports.mallocFree(rand);
  });

  it("won't divide by zero", () => {
    const zero = bigIntToPtr(0n);
    const rand = bigIntToPtr(randomBigInt(128));

    expect(exports.mpDiv(rand, zero)).to.equal(0n);

    exports.mallocFree(zero);
    exports.mallocFree(rand);
  });

  it("can divide smaller numbers by bigger numbers", () => {
    const large = randomBigInt(128);
    const small = randomBigInt(64);
    const large_ptr = bigIntToPtr(large);
    const small_ptr = bigIntToPtr(small);

    const divRes = exports.mpDiv(small_ptr, large_ptr);
    expect(divRes).to.not.equal(0n);

    const temp = new BigUint64Array([divRes]);
    const words = new Uint32Array(temp.buffer);

    const quot = ptrToBigInt(words[1]);
    const rem = ptrToBigInt(words[0]);

    expect(quot).to.equal(0n, "quotient should be zero");
    expect(rem).to.equal(small, "remainder should be dividend");

    exports.mallocFree(words[0]);
    exports.mallocFree(small_ptr);
    exports.mallocFree(large_ptr);
  });

  it("can divide 64-bit numbers", () => {
    for (let i = 0; i != 20; i++) {
      const large = randomBigInt(64, true);
      const small = randomBigInt(48, true);
      const large_ptr = bigIntToPtr(large);
      const small_ptr = bigIntToPtr(small);

      const divRes = exports.mpDiv(large_ptr, small_ptr);
      expect(divRes).to.not.equal(0n);

      const temp = new BigUint64Array([divRes]);
      // const words = new Uint32Array(temp.buffer);
      const quot_ptr = Number(BigInt.asUintN(32, temp[0] >> 32n));
      const rem_ptr = Number(BigInt.asUintN(32, temp[0]));

      const quot = ptrToBigInt(quot_ptr);
      const rem = ptrToBigInt(rem_ptr);

      expect(quot).to.equal(
        large / small,
        `${i}: (quotient ${large} ${small})`
      );
      expect(rem).to.equal(
        large % small,
        `${i}: (remainder ${large} ${small})`
      );

      if (quot != 0n) {
        exports.mallocFree(quot_ptr);
      }
      exports.mallocFree(rem_ptr);
      exports.mallocFree(small_ptr);
      exports.mallocFree(large_ptr);
    }
  });

  it("can divide large-bit numbers by 1", () => {
    for (let i = 0; i != 20; i++) {
      const large = randomBigInt(256, true);
      const large_ptr = bigIntToPtr(large);
      const small_ptr = bigIntToPtr(1n);

      const divRes = exports.mpDiv(large_ptr, small_ptr);
      expect(divRes).to.not.equal(0n);

      const temp = new BigUint64Array([divRes]);
      // const words = new Uint32Array(temp.buffer);
      const quot_ptr = Number(BigInt.asUintN(32, temp[0] >> 32n));
      const rem_ptr = Number(BigInt.asUintN(32, temp[0]));

      const quot = ptrToBigInt(quot_ptr);
      const rem = ptrToBigInt(rem_ptr);

      expect(quot).to.equal(large, `${i}: (quotient ${large} 1)`);
      expect(rem).to.equal(0n, `${i}: (remainder ${large} 1)`);

      if (quot != 0n) {
        exports.mallocFree(quot_ptr);
      }
      exports.mallocFree(rem_ptr);
      exports.mallocFree(small_ptr);
      exports.mallocFree(large_ptr);
    }
  });

  it("can divide large-bit +ve numbers by 64-bit +ve numbers", () => {
    for (let i = 0; i != 20; i++) {
      const large = randomBigInt(256);
      const small = randomBigInt(48);
      const large_ptr = bigIntToPtr(large);
      const small_ptr = bigIntToPtr(small);

      const divRes = exports.mpDiv(large_ptr, small_ptr);
      expect(divRes).to.not.equal(0n);

      const temp = new BigUint64Array([divRes]);
      // const words = new Uint32Array(temp.buffer);
      const quot_ptr = Number(BigInt.asUintN(32, temp[0] >> 32n));
      const rem_ptr = Number(BigInt.asUintN(32, temp[0]));

      const quot = ptrToBigInt(quot_ptr);
      const rem = ptrToBigInt(rem_ptr);

      expect(quot).to.equal(
        large / small,
        `${i}: (quotient ${large} ${small})`
      );
      expect(rem).to.equal(
        large % small,
        `${i}: (remainder ${large} ${small})`
      );

      if (quot != 0n) {
        exports.mallocFree(quot_ptr);
      }
      exports.mallocFree(rem_ptr);
      exports.mallocFree(small_ptr);
      exports.mallocFree(large_ptr);
    }
  });

  it("can divide large-bit numbers by 64-bit numbers", () => {
    for (let i = 0; i != 20; i++) {
      const large = randomBigInt(256, true);
      const small = randomBigInt(48, true);
      const large_ptr = bigIntToPtr(large);
      const small_ptr = bigIntToPtr(small);

      const divRes = exports.mpDiv(large_ptr, small_ptr);
      expect(divRes).to.not.equal(0n);

      const temp = new BigUint64Array([divRes]);
      // const words = new Uint32Array(temp.buffer);
      const quot_ptr = Number(BigInt.asUintN(32, temp[0] >> 32n));
      const rem_ptr = Number(BigInt.asUintN(32, temp[0]));

      const quot = ptrToBigInt(quot_ptr);
      const rem = ptrToBigInt(rem_ptr);

      expect(quot).to.equal(
        large / small,
        `${i}: (quotient ${large} ${small})`
      );
      expect(rem).to.equal(
        large % small,
        `${i}: (remainder ${large} ${small})`
      );

      if (quot != 0n) {
        exports.mallocFree(quot_ptr);
      }
      exports.mallocFree(rem_ptr);
      exports.mallocFree(small_ptr);
      exports.mallocFree(large_ptr);
    }
  });

  it("can shift numbers left", () => {
    for (let i = 0; i != 32; i++) {
      const small = randomBigInt(48, true);
      const small_ptr = bigIntToPtr(small);

      const val_ptr = exports.mpShl(small_ptr, i);
      const val = ptrToBigInt(val_ptr);
      expect(val).to.equal(
        small << BigInt(i),
        `${small.toString(16)} << ${i} == ${(small << BigInt(i)).toString(16)}`
      );

      exports.mallocFree(small_ptr);
      exports.mallocFree(val_ptr);
    }
  });

  it("can shift numbers left by multiples of word-size", () => {
    for (let i = 0; i <= 256; i += 32) {
      const small = randomBigInt(48, true);
      const small_ptr = bigIntToPtr(small);

      const val_ptr = exports.mpShl(small_ptr, i);
      const val = ptrToBigInt(val_ptr);
      expect(val).to.equal(
        small << BigInt(i),
        `${small.toString(16)} << ${i} == ${(small << BigInt(i)).toString(16)}`
      );

      exports.mallocFree(small_ptr);
      exports.mallocFree(val_ptr);
    }
  });

  it("can shift numbers right in place", () => {
    for (let i = 0; i != 32; i++) {
      const small = randomBigInt(48);
      const small_ptr = bigIntToPtr(small);

      exports.mpShrIp(small_ptr, i);
      const val = ptrToBigInt(small_ptr);
      expect(val).to.equal(
        small >> BigInt(i),
        `${small.toString(16)} >> ${i} == ${(small >> BigInt(i)).toString(
          16
        )} got ${val.toString(16)}`
      );

      exports.mallocFree(small_ptr);
    }
  });

  const checkCompare = (
    mpCmp: (left_ptr: number, right_ptr: number) => number,
    refCmp: (left: bigint, right: bigint) => boolean,
    description: string
  ) => {
    const numbers = [
      randomBigInt(48),
      randomBigInt(64),
      randomBigInt(128),
      -randomBigInt(48),
      -randomBigInt(64),
      -randomBigInt(128),
    ];

    for (const left of numbers) {
      const left_ptr = bigIntToPtr(left);
      try {
        expect(mpCmp(left_ptr, left_ptr) == 1).to.equal(
          refCmp(left, left),
          `(${description} ${left} ${left})`
        );

        for (const right of numbers) {
          const right_ptr = bigIntToPtr(right);
          try {
            expect(mpCmp(left_ptr, right_ptr) == 1).to.equal(
              refCmp(left, right),
              `(${description} ${left} ${right})`
            );
          } finally {
            exports.mallocFree(right_ptr);
          }
        }
      } finally {
        exports.mallocFree(left_ptr);
      }
    }
  };

  it("can compare numbers", () => {
    checkCompare(exports.mpEq, (a, b) => a == b, "=");
    checkCompare(exports.mpGt, (a, b) => a > b, ">");
    checkCompare(exports.mpGe, (a, b) => a >= b, ">=");
    checkCompare(exports.mpLt, (a, b) => a < b, "<");
    checkCompare(exports.mpLe, (a, b) => a <= b, "<=");
  });
});
