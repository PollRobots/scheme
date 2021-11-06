import { expect } from "chai";
import "mocha";

import { checkForLeaks, checkMemory, createString, dumpMemory, getString, loadWasm } from "./common";

interface TestExports {
  memory: WebAssembly.Memory;
  malloc_init: () => void;
  malloc_free: (ptr: number) => void;
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
    readerInit: instance.exports.readerInit as () => number,
    readerFree: instance.exports.readerFree as (ptr: number) => void,
    readerReadToken: instance.exports.readerReadToken as (
      ptr: number
    ) => number,
  };
}

class IoEvent {
  private _data?: string;
  private _type: "read" | "write";

  constructor(type: "read" | "write", data?: string) {
    this._type = type;
    this._data = data;
  }

  public get type(): "read" | "write" {
    return this._type;
  }
  public set type(v: "read" | "write") {
    this._type = v;
  }

  public get data(): string | undefined {
    return this._data;
  }

  public set data(v: string | undefined) {
    this._data = v;
  }
}

type IoEventHandler = (evt: IoEvent) => boolean;

class IoTest {
  private exports_: TestExports | null = null;
  private readonly readEvents: IoEventHandler[] = [];
  private readonly writeEvents: IoEventHandler[] = [];

  constructor() {}

  get exports(): TestExports | null {
    return this.exports_;
  }
  set exports(value: TestExports | null) {
    this.exports_ = value;
  }

  addEventListener(type: "read" | "write", callback: IoEventHandler): void {
    if (type === "read") {
      this.readEvents.push(callback);
    } else {
      this.writeEvents.push(callback);
    }
  }

  removeEventListener(type: "read" | "write", callback: IoEventHandler): void {
    const list = type === "read" ? this.readEvents : this.writeEvents;
    const index = list.indexOf(callback);
    if (index >= 0) {
      list.splice(index, 1);
    }
  }

  read(): number {
    if (!this.exports_) {
      return 0;
    }
    const event = new IoEvent("read");
    for (const handler of this.readEvents) {
      const res = handler(event);
      if (event.data != undefined) {
        return createString(this.exports_, event.data);
      } else if (res) {
        return 0;
      }
    }
    return 0;
  }

  write(str: number) {
    if (!this.exports_) {
      return;
    }
    const toWrite = getString(this.exports_, str);
    const event = new IoEvent("write", toWrite);

    for (const handler of this.writeEvents) {
      handler(event);
    }
  }
}

describe("reader wasm", () => {
  const io = new IoTest();
  const wasm = loadWasm({
    read: () => io.read(),
    write: (ptr) => io.write(ptr),
  });
  let exports: TestExports;

  before(async () => {
    const instance = await wasm;
    exports = exportsFromInstance(instance);
    io.exports = exports;
    exports.malloc_init();
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

    exports.malloc_free(first);
    exports.malloc_free(second);
    exports.malloc_free(third);
    exports.malloc_free(fourth);
    exports.malloc_free(fifth);

    expect(words[wptr]).to.equal(input);

    exports.readerFree(reader);

    io.removeEventListener("read", readHandler);
  });
});
