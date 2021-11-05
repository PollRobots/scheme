import { expect } from 'chai';
import fs from 'fs/promises';

interface TestExports {
  memory: WebAssembly.Memory;
  strFrom32: (len: number, val: number) => number;
  strFrom64: (len: number, val: bigint) => number;
  strFrom128: (len: number, val1: bigint, val2: bigint) => number;
}

export async function loadWasm(): Promise<WebAssembly.Instance> {
  const wasm = await fs.readFile("dist/test.wasm");
  try {
    const module = await WebAssembly.instantiate(wasm, {});
    return module.instance;
  } catch (err) {
    console.error(err);
    throw err;
  }
}

export function createString(exports: TestExports, str: string): number {
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

export function getString(exports: TestExports, ptr: number): string {
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

