import { expect } from "chai";
import "mocha";
import { commonExportsFromInstance, CommonTestExports, loadWasm } from "./common";

interface TestExports extends CommonTestExports {
  memory: WebAssembly.Memory;
  mallocZero: (ptr: number, len: number) => void;
  mallocInit: () => void;
  mallocLoadList: (ptr: number) => bigint;
  mallocStoreList: (ptr: number, next: number, size: number) => void;
  malloc: (size: number) => number;
  mallocFree: (ptr: number) => void;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    ...commonExportsFromInstance(instance),
    mallocZero: instance.exports.mallocZero as (
      ptr: number,
      len: number
    ) => void,
    mallocInit: instance.exports.mallocInit as () => void,
    mallocLoadList: instance.exports.mallocLoadList as (ptr: number) => bigint,
    mallocStoreList: instance.exports.mallocStoreList as (
      ptr: number,
      next: number,
      size: number
    ) => void,
    malloc: instance.exports.malloc as (size: number) => number,
    mallocFree: instance.exports.mallocFree as (ptr: number) => void,
  };
}

describe("malloc wasm", () => {
  const wasm = loadWasm();

  it("should have an export called mallocZero", async () => {
    const instance = await wasm;
    expect(instance.exports).has.property("mallocZero");
    expect(instance.exports.mallocZero).is.instanceOf(Function);
  });

  it("should have an export called memory", async () => {
    const instance = await wasm;
    expect(instance.exports).has.property("memory");
    expect(instance.exports.memory).is.instanceOf(WebAssembly.Memory);
  });

  it("should zero memory within bounds", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    const memView = new Uint8Array(exports.memory.buffer);
    const local = new Uint8Array(exports.memory.buffer.byteLength);
    local.fill(0xff);
    memView.set(local);

    const expected = new Uint8Array(80);
    expected.fill(0xff);
    expect(Array.from(memView.slice(0, 80))).has.ordered.members(
      Array.from(expected.values())
    );

    for (let i = 10; i < 60; i++) {
      expected[i] = 0;
    }

    exports.mallocZero(10, 50);
    expect(Array.from(memView.slice(0, 80))).has.ordered.members(
      Array.from(expected.values())
    );
  });

  it("should initialize memory", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    expect(instance.exports).has.property("mallocInit");
    expect(instance.exports.mallocInit).is.instanceOf(Function);

    exports.mallocInit();
    const view = new Uint32Array(exports.memory.buffer);

    // free list ptr should be immediately after the malloc header
    // malloc header offset + malloc header size
    expect(view[8]).to.equal(32 + 8);
    // memory size
    expect(view[9]).to.equal(exports.memory.buffer.byteLength);

    // all memory should be a free list item
    // next ptr == 0 for end of list
    expect(view[10]).to.equal(0);
    // free block size = memsize - offset (40) - hdrsize(8)
    expect(view[11]).to.equal(exports.memory.buffer.byteLength - 48);
  });

  it("should round trip through load list and store list", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    expect(instance.exports).has.property("mallocLoadList");
    expect(instance.exports.mallocLoadList).is.instanceOf(Function);
    expect(instance.exports).has.property("mallocStoreList");
    expect(instance.exports.mallocStoreList).is.instanceOf(Function);

    exports.mallocStoreList(128, 256, 1234);
    const memview = new Uint32Array(exports.memory.buffer);
    expect(memview[128 / 4]).to.equal(256);
    expect(memview[132 / 4]).to.equal(1234);

    const res = exports.mallocLoadList(128);
    expect(Number(BigInt.asUintN(32, res >> 32n))).to.equal(256);
    expect(Number(BigInt.asUintN(32, res))).to.equal(1234);
  });

  it("should panic when freeing an invalid ptr", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    expect(instance.exports).has.property("mallocFree");
    expect(instance.exports.mallocFree).is.instanceOf(Function);

    expect(() => exports.mallocFree(0)).to.throw("unreachable");

    exports.mallocInit();
    expect(() => exports.mallocFree(40)).to.throw("unreachable");
  });

  it("should allocate and free memory while maintaining consistency", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    expect(instance.exports).has.property("malloc");
    expect(instance.exports.malloc).is.instanceOf(Function);

    exports.mallocInit();

    const ptrs: { ptr: number; size: number }[] = [];
    const allocate = (view: Uint8Array, words: Uint32Array) => {
      const size = (10 + 246 * Math.random()) | 0;
      const ptr = exports.malloc(size);
      // console.log(`malloc(${size}) => ${ptr}`);

      expect(ptr & 0x7).to.equal(0);

      const hdr = words.slice((ptr - 8) / 4, ptr / 4);
      expect(hdr[0]).to.equal(
        ptr - 8,
        "next ptr should be self ref for allocated memory"
      );
      expect(hdr[1]).to.equal(
        (size + 7) & ~7,
        `size should be rounded to next multiple of 8 ${ptr} ${size} ${hdr[0]} ${hdr[1]}`
      );
      for (let i = 0; i != size; i++) {
        view[ptr + i] = 0xff;
      }

      expect(ptrs.some((el) => el.ptr === ptr)).to.be.false;
      ptrs.push({ ptr: ptr, size: size });
    };

    const free = (view: Uint8Array, words: Uint32Array) => {
      const index = ((ptrs.length * Math.random()) | 0) % ptrs.length;
      const { ptr, size } = ptrs[index];

      const hdr = words.slice((ptr - 8) / 4, ptr / 4);
      expect(hdr[0]).to.equal(
        ptr - 8,
        "next ptr should be self ref for allocated memory"
      );
      expect(hdr[1]).to.equal(
        (size + 7) & ~7,
        "size should be rounded to next multiple of 8"
      );

      const slice = view.slice(ptr, ptr + size);
      expect(Array.from(slice)).has.ordered.members(
        new Array(size).fill(0xff),
        `memory should not have been modified, ${ptr}`
      );
      words.slice(ptr / 4, (ptr + hdr[1]) / 4).fill(0xdeadbeef);

      try {
        for (let i = 0; i != size; i++) {
          view[ptr + i] = 0xdd;
        }
        // console.log(`free(${ptr})`);
        exports.mallocFree(ptr);
      } catch (err) {
        console.error(err);
        throw err;
      }
      ptrs.splice(index, 1);
    };

    for (let i = 0; i < 100; i++) {
      const memory = instance.exports.memory as WebAssembly.Memory;
      const view = new Uint8Array(memory.buffer);
      const words = new Uint32Array(memory.buffer);

      // validate heap

      // console.log(`${i.toString().padStart(2, "0")} Check Heap`);
      // check free list
      let offset = 8;
      let curr = words[offset];
      expect(words[9]).to.equal(
        view.byteLength,
        "second word of malloc header should be size of memory"
      );
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
        expect(c_size & 7).to.equal(
          0,
          "free block size should be divisible by 8"
        );

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
          alloc += a_size + 8;
        }

        curr = c_next;
      }
      // console.log("---------------");
      // check allocations
      for (const { ptr, size } of ptrs) {
        const hdr = words.slice((ptr - 8) / 4, ptr / 4);
        expect(hdr[0]).to.equal(
          ptr - 8,
          `next ptr should be self ref ${ptr}:(${size}) ${hdr[0]}:(${hdr[1]})`
        );
        expect(hdr[1]).to.equal(
          (size + 7) & ~7,
          `size should be rounded to next multiple of 8 ${ptr}:(${size}) ${hdr[0]}:(${hdr[1]})`
        );
        const slice = view.slice(ptr, ptr + size);
        if (slice.some((el) => el != 0xff)) {
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
        expect(Array.from(slice)).has.ordered.members(
          new Array(size).fill(0xff),
          `Memory at ${ptr}(${size}) should not have been modified`
        );
      }

      if (Math.random() < Math.cos((i * Math.PI) / 200)) {
        allocate(view, words);
      } else if (ptrs.length) {
        free(view, words);
      }
    }

    for (const { ptr } of ptrs) {
      const memory = instance.exports.memory as WebAssembly.Memory;
      const view = new Uint8Array(memory.buffer);
      const words = new Uint32Array(memory.buffer);
      free(view, words);
    }
  });
});
