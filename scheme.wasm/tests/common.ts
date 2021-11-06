import { expect } from "chai";
import { Console } from "console";
import fs from "fs/promises";

interface TestExports {
  memory: WebAssembly.Memory;
  strFrom32: (len: number, val: number) => number;
  strFrom64: (len: number, val: bigint) => number;
  strFrom128: (len: number, val1: bigint, val2: bigint) => number;
}

export interface IoModule extends Record<string, Function> {
  read: () => number;
  write: (ptr: number) => void;
}

export async function loadWasm(io?: IoModule): Promise<WebAssembly.Instance> {
  const wasm = await fs.readFile("dist/test.wasm");
  try {
    const imports: WebAssembly.Imports = {};

    imports["io"] = io || {
      read: () => {
        console.warn("READ");
        return 0;
      },
      write: (ptr: number) => {
        console.warn(`WRITE: 0x${ptr.toString(16).padStart(8, "0")}`);
      },
    };

    const module = await WebAssembly.instantiate(wasm, imports);
    return module.instance;
  } catch (err) {
    console.error(err);
    throw err;
  }
}

export function createString(exports: TestExports, str: string): number {
  const ptr = createStringImpl(exports, str);
  checkMemory(exports);
  return ptr;
}

export function createStringImpl(exports: TestExports, str: string): number {
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

export function checkMemory(
  exports: TestExports,
  checkForLeaks: boolean = false
) {
  const view = new Uint8Array(exports.memory.buffer);
  const words = new Uint32Array(exports.memory.buffer);

  // console.log(`${i.toString().padStart(2, "0")} Check Heap`);
  // check free list
  let offset = 8;
  let curr = words[offset];
  expect(words[9]).to.equal(
    view.byteLength,
    "second word of malloc header should be size of memory"
  );

  const allocations: { ptr: number; size: number }[] = [];

  // console.log("---------------");
  while (curr != 0) {
    offset = curr / 4;
    const c_next = words[offset];
    const c_size = words[offset + 1];
    // console.log(
    //   `Free list at ${curr}..${
    //     curr + c_size + 8
    //   } = {next: ${c_next}, size: ${c_size}}`
    // );
    expect(c_size & 7).to.equal(0, "free block size should be divisible by 8");

    if (c_next != 0) {
      expect(c_next).to.be.greaterThan(
        curr,
        "next should be greater than curr"
      );
      expect(curr + c_size + 8).to.be.lessThanOrEqual(
        c_next,
        "next should be beyond curr + size + 8"
      );
      expect(curr + c_size + 8).to.not.equal(c_next, "Failed merge!!");
    } else {
      expect(curr + c_size + 8).to.be.lessThanOrEqual(view.byteLength);
    }
    // check for allocated blocks between curr + size + 8 and c_next
    let alloc = curr + c_size + 8;
    const limit = c_next ? c_next : view.byteLength;
    while (alloc < limit) {
      const hdr = words.slice(alloc / 4, (alloc + 8) / 4);
      const a_ptr = hdr[0];
      const a_size = hdr[1];
      // console.log(
      //   `Alloc at ${alloc}..${
      //     alloc + a_size + 8
      //   } = {ptr: ${a_ptr}, size: ${a_size}}`
      // );

      allocations.push({ ptr: alloc + 8, size: a_size });
      alloc += a_size + 8;
    }

    curr = c_next;
  }

  if (allocations.length) {
    for (const { ptr, size } of allocations) {
      const hdr = words.slice((ptr - 8) / 4, ptr / 4);
      expect(hdr[0]).to.equal(
        ptr - 8,
        `next ptr should be self ref ${ptr}:(${size}) ${hdr[0]}:(${hdr[1]})`
      );
      expect(hdr[1]).to.equal(
        (size + 7) & ~7,
        `size should be rounded to next multiple of 8 ${ptr}:(${size}) ${hdr[0]}:(${hdr[1]})`
      );

      if (checkForLeaks) {
        const slice = view.slice(ptr, ptr + size);
        console.log(
          `Bad alloc at ${ptr}..${ptr + size} = {ptr: ${ptr}, size: ${size}}`
        );
        console.log(`ptr: ${hdr[0]}, size: ${hdr[1]}`);
        for (let ii = 0; ii < size; ii += 16) {
          const line = Array.from(slice.slice(ii, Math.min(ii + 16, size)));
          const hexes = line.map((el) => el.toString(16).padStart(2, "0"));
          while (hexes.length < 16) {
            hexes.push("--");
          }
          const chars = line.map((el) =>
            el >= 0x20 && el < 0x80 ? String.fromCodePoint(el) : "."
          );
          console.log(
            `${(ii + ptr).toString(16).padStart(5, "0")}  ${hexes.join(
              " "
            )}  ${chars.join("")}`
          );
        }
      }
    }
  }

  if (checkForLeaks) {
    expect(allocations.length).to.equal(0, "There should be no allocations");
  }
}

export function checkForLeaks(exports: TestExports) {
  return checkMemory(exports, true);
}

export function dumpMemory(exports: TestExports) {
  const view = new Uint8Array(exports.memory.buffer);
  const words = new Uint32Array(exports.memory.buffer);

  // check free list
  let offset = 8;
  let curr = words[offset];

  const allocations: { ptr: number; size: number }[] = [];

  console.log("---------------");
  while (curr != 0) {
    offset = curr / 4;
    const c_next = words[offset];
    const c_size = words[offset + 1];
    console.log(
      `Free list at ${curr}..${
        curr + c_size + 8
      } = {next: ${c_next}, size: ${c_size}}`
    );
    expect(c_size & 7).to.equal(0, "free block size should be divisible by 8");

    if (c_next != 0) {
      expect(c_next).to.be.greaterThan(
        curr,
        "next should be greater than curr"
      );
      expect(curr + c_size + 8).to.be.lessThanOrEqual(
        c_next,
        "next should be beyond curr + size + 8"
      );
      expect(curr + c_size + 8).to.not.equal(c_next, "Failed merge!!");
    } else {
      expect(curr + c_size + 8).to.be.lessThanOrEqual(view.byteLength);
    }
    // check for allocated blocks between curr + size + 8 and c_next
    let alloc = curr + c_size + 8;
    const limit = c_next ? c_next : view.byteLength;
    while (alloc < limit) {
      const hdr = words.slice(alloc / 4, (alloc + 8) / 4);
      const a_ptr = hdr[0];
      const a_size = hdr[1];

      allocations.push({ ptr: alloc + 8, size: a_size });
      alloc += a_size + 8;
    }

    curr = c_next;
  }

  if (allocations.length) {
    for (const { ptr, size } of allocations) {
      const hdr = words.slice((ptr - 8) / 4, ptr / 4);

      const slice = view.slice(ptr, ptr + size);
      console.log(
        `Alloc at ${ptr}..${ptr + size} = {ptr: ${ptr}, size: ${size}}`
      );
      console.log(`ptr: ${hdr[0]}, size: ${hdr[1]}`);
      for (let ii = 0; ii < size; ii += 16) {
        const line = Array.from(slice.slice(ii, Math.min(ii + 16, size)));
        const hexes = line.map((el) => el.toString(16).padStart(2, "0"));
        while (hexes.length < 16) {
          hexes.push("--");
        }
        const chars = line.map((el) =>
          el >= 0x20 && el < 0x80 ? String.fromCodePoint(el) : "."
        );
        console.log(
          `${(ii + ptr).toString(16).padStart(5, "0")}  ${hexes.join(
            " "
          )}  ${chars.join("")}`
        );
      }
    }
  }
}
