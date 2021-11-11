import { expect } from "chai";
import "mocha";

import {
  checkForLeaks,
  checkMemory,
  createString,
  dumpMemory,
  getString,
  loadWasm,
} from "./common";

interface TestExports {
  memory: WebAssembly.Memory;
  mallocInit: () => void;
  mallocFree: (ptr: number) => void;
  strFrom32: (len: number, val: number) => number;
  strFrom64: (len: number, val: bigint) => number;
  strFrom128: (len: number, val1: bigint, val2: bigint) => number;
  heapCreate: (size: number) => number;
  heapDestroy: (ptr: number) => void;
  heapAlloc: (
    heap: number,
    type: number,
    data1: number,
    data2: number
  ) => number;
  heapFree: (heap: number, ptr: number) => void;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    memory: instance.exports.memory as WebAssembly.Memory,
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
    heapCreate: instance.exports.heapCreate as (size: number) => number,
    heapDestroy: instance.exports.heapDestroy as (ptr: number) => void,
    heapAlloc: instance.exports.heapAlloc as (
      heap: number,
      type: number,
      data1: number,
      data2: number
    ) => number,
    heapFree: instance.exports.heapFree as (heap: number, ptr: number) => void,
  };
}

describe("heap wasm", () => {
  const wasm = loadWasm();
  let exports: TestExports;

  before(async () => {
    const instance = await wasm;
    exports = exportsFromInstance(instance);
    exports.mallocInit();
  });

  after(() => {
    checkForLeaks(exports);
  });

  it("can create a heap", () => {
    const view = new Uint8Array(exports.memory.buffer);

    const heapSize = 8;
    const heap = exports.heapCreate(heapSize);
    expect(heap).to.be.greaterThanOrEqual(40);
    expect(heap).to.be.lessThan(view.byteLength);

    const heapAllocSize = heapSize * 12 + 12; // entries are 12 bytes long, the header is 12 bytes

    const words = new Uint32Array(
      exports.memory.buffer.slice(heap, heap + heapAllocSize)
    );
    expect(words[0]).to.equal(
      heapSize,
      "The size in the header should be as requested"
    );

    expect(words[1]).to.equal(
      heap + 12,
      "The free ptr should point to the first entry"
    );

    for (let i = 0; i < heapSize; i++) {
      expect(words[i * 3 + 3]).to.equal(0, "The type should be Empty(0)");
      if (i == heapSize - 1) {
        expect(words[i * 3 + 4]).to.equal(
          0,
          "The last entry should have a next-ptr of 0"
        );
      } else {
        expect(words[i * 3 + 4]).to.equal(
          heap + (i + 2) * 12,
          "Each entry should point to the next"
        );
      }
    }

    exports.mallocFree(heap);
  });

  it("can store and retrieve elements from a heap", () => {
    const view = new Uint8Array(exports.memory.buffer);

    const heapSize = 4;
    const heap = exports.heapCreate(heapSize);

    const kTestData: { type: number; data1: number; data2: number }[] = [
      { type: 1, data1: 0x20656e4f, data2: 3 },
      { type: 1, data1: 0x206f7754, data2: 9 },
      { type: 1, data1: 0x73657254, data2: 27 },
      { type: 1, data1: 0x72756f46, data2: 81 },
      { type: 1, data1: 0x65766946, data2: 243 },
      { type: 1, data1: 0x20786953, data2: 729 },
      { type: 1, data1: 0x6e766553, data2: 2187 },
      { type: 1, data1: 0x6f68634f, data2: 6561 },
      { type: 1, data1: 0x656e694e, data2: 19683 },
      { type: 1, data1: 0x206e6554, data2: 59049 },
    ];
    const ptrs: { ptr: number; type: number; data1: number; data2: number }[] =
      [];

    for (const { type, data1, data2 } of kTestData) {
      const ptr = exports.heapAlloc(heap, type, data1, data2);
      ptrs.push({ ptr: ptr, type: type, data1: data1, data2: data2 });
    }

    for (const { ptr, type, data1, data2 } of ptrs) {
      const words = new Uint32Array(exports.memory.buffer.slice(ptr, ptr + 12));
      expect(words[0]).to.equal(type);
      expect(words[1]).to.equal(data1);
      expect(words[2]).to.equal(data2);
    }

    expect(() => exports.heapAlloc(heap, 0, 0, 0)).to.throw(
      "unreachable",
      "Cannot create a heap item with type empty(0)"
    );
    expect(() => exports.heapAlloc(heap, 100, 0, 0)).to.throw(
      "unreachable",
      "Cannot create a heap item with an unknown type"
    );

    exports.heapDestroy(heap);
  });

  it("can store and free elements from a heap", () => {
    const view = new Uint8Array(exports.memory.buffer);

    const heapSize = 4;
    const heap = exports.heapCreate(heapSize);

    const kTestData: { type: number; data1: number; data2: number }[] = [
      { type: 1, data1: 0x20656e4f, data2: 3 },
      { type: 1, data1: 0x206f7754, data2: 9 },
      { type: 1, data1: 0x73657254, data2: 27 },
      { type: 1, data1: 0x72756f46, data2: 81 },
      { type: 1, data1: 0x65766946, data2: 243 },
      { type: 1, data1: 0x20786953, data2: 729 },
      { type: 1, data1: 0x6e766553, data2: 2187 },
      { type: 1, data1: 0x6f68634f, data2: 6561 },
      { type: 1, data1: 0x656e694e, data2: 19683 },
      { type: 1, data1: 0x206e6554, data2: 59049 },
    ];
    const ptrs: { ptr: number; type: number; data1: number; data2: number }[] =
      [];

    for (const { type, data1, data2 } of kTestData) {
      const ptr = exports.heapAlloc(heap, type, data1, data2);
      ptrs.push({ ptr: ptr, type: type, data1: data1, data2: data2 });
    }

    for (const { ptr } of ptrs) {
      exports.heapFree(heap, ptr);
      const words = new Uint32Array(exports.memory.buffer.slice(ptr, ptr + 12));
      expect(words[0]).to.equal(0);
    }

    expect(() => exports.heapFree(heap, heap)).to.throw(
      "unreachable",
      "Cannot free an item that isn't in a heap"
    );

    exports.heapDestroy(heap);
  });
});
