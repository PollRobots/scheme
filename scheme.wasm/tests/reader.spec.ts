import { expect } from "chai";
import "mocha";

import {
  checkForLeaks,
  checkMemory,
  createString,
  dumpMemory,
  getString,
  IoEvent,
  IoModule,
  IoTest,
  loadWasm,
} from "./common";

interface TestExports {
  memory: WebAssembly.Memory;
  mallocInit: () => void;
  mallocFree: (ptr: number) => void;
  strFrom32: (len: number, val: number) => number;
  strFrom64: (len: number, val: bigint) => number;
  strFrom128: (len: number, val1: bigint, val2: bigint) => number;
  readerInit: () => number;
  readerFree: (ptr: number) => void;
  readerReadToken: (ptr: number) => number;
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
    readerInit: instance.exports.readerInit as () => number,
    readerFree: instance.exports.readerFree as (ptr: number) => void,
    readerReadToken: instance.exports.readerReadToken as (
      ptr: number
    ) => number,
  };
}

describe("reader wasm", () => {
  const io = new IoTest();
  const wasm = loadWasm(io.module);
  let exports: TestExports;

  before(async () => {
    const instance = await wasm;
    exports = exportsFromInstance(instance);
    io.exports = exports;
    exports.mallocInit();
  });

  after(() => {
    checkForLeaks(exports);
  });

  it("Can allocate and initialize a reader", () => {
    const reader = exports.readerInit();
    expect(reader).to.be.greaterThanOrEqual(40);
    expect(reader).to.be.lessThan(exports.memory.buffer.byteLength);

    const words = new Uint32Array(exports.memory.buffer);
    const wptr = reader / 4;
    expect(words[wptr]).to.equal(0);
    expect(words[wptr + 1]).to.equal(0);
    expect(words[wptr + 2]).to.be.greaterThanOrEqual(40);
    expect(words[wptr + 2]).to.be.lessThan(exports.memory.buffer.byteLength);
    expect(words[wptr + 3]).to.equal(16, "accum should start with 16 entries");

    exports.readerFree(reader);
  });

  it("reads simple tokens from an input string", () => {
    const reader = exports.readerInit();
    const words = new Uint32Array(exports.memory.buffer);
    const wptr = reader / 4;
    expect(words[wptr]).to.equal(0);

    const inputString = "one € (three) ";

    const readHandler = (evt: IoEvent) => {
      evt.data = inputString;
      return true;
    };

    io.addEventListener("read", readHandler);

    checkMemory(exports);

    const first = exports.readerReadToken(reader);
    expect(getString(exports, first)).to.equal("one");

    const input = words[wptr];
    expect(input).to.not.equal(0);
    expect(getString(exports, input)).to.equal(inputString);

    const second = exports.readerReadToken(reader);
    expect(getString(exports, second)).to.equal("€");
    const third = exports.readerReadToken(reader);
    expect(getString(exports, third)).to.equal("(");
    const fourth = exports.readerReadToken(reader);
    expect(getString(exports, fourth)).to.equal("three");
    const fifth = exports.readerReadToken(reader);
    expect(getString(exports, fifth)).to.equal(")");

    expect(words[wptr]).to.equal(input);

    exports.mallocFree(first);
    exports.mallocFree(second);
    exports.mallocFree(third);
    exports.mallocFree(fourth);
    exports.mallocFree(fifth);

    expect(words[wptr]).to.equal(input);

    exports.readerFree(reader);

    io.removeEventListener("read", readHandler);
  });
});
