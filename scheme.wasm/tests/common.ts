import { expect } from "chai";
import fs from "fs/promises";
import path from "path";
import zlib from "zlib";
import crypto from "crypto";

export interface CommonTestExports {
  memory: WebAssembly.Memory;
  malloc: (len: number) => number;
  mallocFree: (ptr: number) => void;
  mallocInit: () => void;
  runtimeInit: () => void;
  runtimeCleanup: () => void;
  gHeap: () => number;
  gNil: () => number;
  gTrue: () => number;
  gFalse: () => number;
  strFrom32: (len: number, val: number) => number;
  strFrom64: (len: number, val: bigint) => number;
  strFrom128: (len: number, val1: bigint, val2: bigint) => number;
  strFromCodePoints: (ptr: number, len: number) => number;
  heapAlloc: (
    heap: number,
    type: number,
    data1: number,
    data2: number
  ) => number;
  heapAllocError: (symbol: number, message: number) => number;
}

const enum SchemeType {
  Empty = 0,
  Nil = 1,
  Boolean = 2,
  Cons = 3,
  I64 = 4,
  F64 = 5,
  Symbol = 6,
  Str = 7,
  Char = 8,
  Env = 9,
  Special = 10,
  Builtin = 11,
  Lambda = 12,
  Error = 13,
  Values = 14,
  Vector = 15,
  Bytevector = 16,
  Cont = 17,
  BigInt = 18,
  Except = 19,
  ContProc = 20,
  MaxHeap = 20,
  Mask = 0x1f,
}

export function commonExportsFromInstance(
  instance: WebAssembly.Instance
): CommonTestExports {
  return {
    memory: instance.exports.memory as WebAssembly.Memory,
    malloc: instance.exports.malloc as (len: number) => number,
    mallocFree: instance.exports.mallocFree as (ptr: number) => void,
    mallocInit: instance.exports.mallocInit as () => void,
    runtimeInit: instance.exports.runtimeInit as () => void,
    runtimeCleanup: instance.exports.runtimeCleanup as () => void,
    gHeap: () => (instance.exports.gHeap as WebAssembly.Global).value as number,
    gNil: () => (instance.exports.gNil as WebAssembly.Global).value as number,
    gTrue: () => (instance.exports.gTrue as WebAssembly.Global).value as number,
    gFalse: () =>
      (instance.exports.gFalse as WebAssembly.Global).value as number,
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
    strFromCodePoints: instance.exports.strFromCodePoints as (
      ptr: number,
      len: number
    ) => number,
    heapAlloc: instance.exports.heapAlloc as (
      heap: number,
      type: number,
      data1: number,
      data2: number
    ) => number,
    heapAllocError: instance.exports.heapAllocError as (
      symbol: number,
      message: number
    ) => number,
  };
}

export interface IoModule extends Record<string, Function> {
  read: () => number;
  write: (ptr: number) => void;
}

export interface PortModule extends Record<string, Function> {
  close: (fd: number) => void;
  open: (name: number, mode: number) => number;
}

export interface ProcessModule extends Record<string, Function> {
  exit: (exitCode: number) => void;
  getEnvironmentVariable: (name: number) => number;
  getEnvironmentVariables: () => number;
  setEnvironmentVariable: (name: number, value: number) => void;
  commandLine: () => number;
}

export class ProcessTest {
  private exports_: CommonTestExports | undefined;
  private readonly env_: Map<string, string> = new Map();

  public get exports(): CommonTestExports {
    if (!this.exports_) {
      throw new Error("Invalid Operation");
    }
    return this.exports_;
  }

  public set exports(v: CommonTestExports) {
    this.exports_ = v;
  }

  exit(exitCode: number) {
    console.warn(`(exit ${exitCode})`);
  }

  getEnvironmentVariable(name: number) {
    if (this.exports_ === undefined) {
      return 0;
    }

    const str = getString(this.exports_, name);
    const value = this.env_.get(str);
    if (typeof value === "string") {
      return createHeapString(this.exports_, value);
    } else {
      return 0;
    }
  }

  getEnvironmentVariables() {
    if (this.exports_ === undefined) {
      return 0;
    }

    let tail = this.exports.gNil();
    for (const entry of this.env_.entries()) {
      const item = this.exports_.heapAlloc(
        this.exports_.gHeap(),
        SchemeType.Cons,
        createHeapString(this.exports_, entry[0]),
        createHeapString(this.exports_, entry[1])
      );
      tail = this.exports_.heapAlloc(
        this.exports_.gHeap(),
        SchemeType.Cons,
        item,
        tail
      );
    }
    return tail;
  }

  setEnvironmentVariable(name: number, value: number) {
    if (this.exports_ === undefined) {
      return;
    }

    this.env_.set(
      getString(this.exports_, name),
      getString(this.exports_, value)
    );
  }

  commandLine(): number {
    if (this.exports_ === undefined) {
      return 0;
    }

    return this.exports_.heapAlloc(
      this.exports_.gHeap(),
      SchemeType.Cons,
      createHeapString(this.exports_, "scheme.wasm"),
      this.exports.gNil()
    );
  }

  get module(): ProcessModule {
    return {
      exit: (exitCode) => this.exit(exitCode),
      getEnvironmentVariable: (name) => this.getEnvironmentVariable(name),
      getEnvironmentVariables: () => this.getEnvironmentVariables(),
      setEnvironmentVariable: (name, value) =>
        this.setEnvironmentVariable(name, value),
      commandLine: () => this.commandLine(),
    };
  }
}

export interface UnicodeModule extends Record<string, Function> {
  loadData: (block: number, ptr: number) => void;
}

export class TestUnicode {
  private exports_: CommonTestExports | undefined;
  private readonly unicodeData_: Record<number, ArrayBuffer> = {};

  public get exports(): CommonTestExports {
    if (!this.exports_) {
      throw new Error("Invalid Operation");
    }
    return this.exports_;
  }
  public set exports(v: CommonTestExports) {
    this.exports_ = v;
  }

  async loadUnicodeBlocks() {
    const fullpath = path.resolve(__dirname, "../dist/unicode/blocks.json.gz");
    const compressed = await fs.readFile(fullpath);
    const decompressed = await new Promise<string>((resolve, reject) => {
      zlib.gunzip(compressed, (err, buf) => {
        if (err) {
          reject(err);
        } else {
          resolve(buf.toString("utf-8"));
        }
      });
    });

    const data = JSON.parse(decompressed);
    for (const key of Object.keys(data)) {
      const blockIndex = Number(key);
      if (isNaN(blockIndex)) {
        continue;
      }
      const blockData = Buffer.from(data[key], "base64");
      this.unicodeData_[blockIndex] = blockData;
    }
  }

  loadData(block: number, ptr: number) {
    const data = this.unicodeData_[block];
    if (!data) {
      throw new Error(
        `No unicode data for ${block.toString(16).padStart(4, "0")}`
      );
    }
    pokeMemory(this.exports, ptr, this.unicodeData_[block]);
  }

  get module(): UnicodeModule {
    return {
      loadData: (block, ptr) => this.loadData(block, ptr),
    };
  }
}

export interface FileModule extends Record<string, Function> {
  read: (filenamePtr: number) => number;
}

export interface TimeModule extends Record<string, Function> {
  current: () => number;
}

export async function loadWasm(
  modules: {
    io?: IoModule;
    port?: PortModule;
    process?: ProcessModule;
    unicode?: UnicodeModule;
    file?: FileModule;
    time?: TimeModule;
  } = {}
): Promise<WebAssembly.Instance> {
  const wasm = await fs.readFile("dist/test.wasm");
  try {
    const imports: WebAssembly.Imports = {};

    imports["io"] = modules.io || {
      read: () => {
        console.warn("READ");
        return 0;
      },
      write: (ptr: number) => {
        console.warn(`WRITE: 0x${ptr.toString(16).padStart(8, "0")}`);
      },
    };
    imports["port"] = modules.port || {
      close: (fd: number) => {
        console.warn(`(close-port ${fd})`);
      },
      open: (name: number, mode: number) => {
        console.warn(`(open 0x${name.toString(16).padStart(8, "0")} ${mode})`);
      },
    };
    imports["process"] = modules.process || {
      exit: (exitCode: number) => {
        console.warn(`EXIT: ${exitCode}`);
      },
      getEnvironmentVariable: (name: number) => {
        console.warn(
          `(get-environment-variable 0x${name.toString(16).padStart(8, "0")})`
        );
        return 0;
      },
      getEnvironmentVariables: () => {
        console.warn("(get-environment-variables)");
        return 0;
      },
      setEnvironmentVariable: (name: number, value: number) => {
        console.warn(
          `(set-environment-variable 0x${name
            .toString(16)
            .padStart(8, "0")} 0x${value.toString(16).padStart(8, "0")})`
        );
      },
      commandLine: () => {
        console.warn("(command-line)");
        return;
      },
    };
    imports["unicode"] = modules.unicode || {
      loadData: (block: number, ptr: number) => {
        const blockPath = `../dist/unicode/${block
          .toString(16)
          .padStart(4, "0")}.bin`;
        throw new Error(
          `unicode.loadData not implemented, looking for ${blockPath}`
        );
      },
    };
    imports["dbg"] = {
      data: (a: number, b: number, c: number) => {
        console.log(`${a}: ${b} #x${c.toString(16).padStart(5, "0")} `);
      },
    };
    imports["file"] = modules.file || {
      read: (filenamePtr: number) => {
        console.log(`file-read ${filenamePtr.toString(16)}`);
        return filenamePtr;
      },
    };
    imports["time"] = modules.time || {
      currentSecond: () => Date.now() / 1000,
      currentJiffy: () => Math.round(performance.now()),
      jiffiesPerSecond: () => 1000,
    };

    const module = await WebAssembly.instantiate(wasm, imports);
    return module.instance;
  } catch (err) {
    console.error(err);
    throw err;
  }
}

export function pokeMemory(
  exports: CommonTestExports,
  ptr: number,
  data: ArrayBuffer
) {
  const byteArray = new Uint8Array(data);
  const view = new Uint8Array(exports.memory.buffer);
  view.set(byteArray, ptr);
}

export function createString(exports: CommonTestExports, str: string): number {
  const ptr = createStringImpl(exports, str);
  checkMemory(exports);
  return ptr;
}

export function createHeapError(
  exports: CommonTestExports,
  symbol: string,
  message: string
) {
  return exports.heapAllocError(
    createString(exports, symbol),
    createString(exports, message)
  );
}

export function createStringImpl(
  exports: CommonTestExports,
  str: string
): number {
  const array = new TextEncoder().encode(str);
  if (array.byteLength > 16) {
    const codePoints = new Uint32Array(
      Array.from(str).map((el) => el.codePointAt(0) || 0xfffd)
    );
    const byteArray = new Uint8Array(codePoints.buffer);
    const ptr = exports.malloc(codePoints.length * 4);
    const view = new Uint8Array(exports.memory.buffer);
    view.set(byteArray, ptr);
    const strPtr = exports.strFromCodePoints(ptr, codePoints.length);
    exports.mallocFree(ptr);
    return strPtr;
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

export function getString(exports: CommonTestExports, ptr: number): string {
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

export function createHeapSymbol(
  exports: CommonTestExports,
  str: string
): number {
  const ptr = createString(exports, str);
  return exports.heapAlloc(exports.gHeap(), SchemeType.Symbol, ptr, 0);
}

export function createHeapString(
  exports: CommonTestExports,
  str: string
): number {
  const ptr = createString(exports, str);
  return exports.heapAlloc(exports.gHeap(), SchemeType.Str, ptr, 0);
}

export function checkMemory(
  exports: CommonTestExports,
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
      if (hdr[0] != ptr - 8 || hdr[1] != ((size + 7) & ~7)) {
        dumpAlloc(hdr, size, ptr);
        expect(hdr[0]).to.equal(
          ptr - 8,
          `next ptr should be self ref ${ptr}:(${size}) ${hdr[0]}:(${hdr[1]})`
        );
        expect(hdr[1]).to.equal(
          (size + 7) & ~7,
          `size should be rounded to next multiple of 8 ${ptr}:(${size}) ${hdr[0]}:(${hdr[1]})`
        );
      }

      if (checkForLeaks) {
        console.log(
          `Leaked alloc at ${ptr}..${ptr + size} = {ptr: ${ptr}, size: ${size}}`
        );
        dumpAlloc(hdr, size, ptr);
      }
    }
  }

  if (checkForLeaks) {
    expect(allocations.length).to.equal(0, "There should be no allocations");
  }

  function dumpAlloc(hdr: Uint32Array, size: number, ptr: number) {
    const slice = view.slice(ptr, ptr + size);
    console.log(
      `ptr: #x${hdr[0].toString(16).padStart(5, "0")}, size: ${hdr[1]}`
    );
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

export function checkForLeaks(exports: CommonTestExports) {
  return checkMemory(exports, true);
}

export function dumpMemory(exports: CommonTestExports) {
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

export class IoEvent {
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

export type IoEventHandler = (evt: IoEvent) => boolean;

export class IoTest {
  private exports_: CommonTestExports | null = null;
  private readonly readEvents: IoEventHandler[] = [];
  private readonly writeEvents: IoEventHandler[] = [];

  constructor() {}

  get exports(): CommonTestExports | null {
    return this.exports_;
  }
  set exports(value: CommonTestExports | null) {
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

  get module(): IoModule {
    return {
      read: () => this.read(),
      write: (ptr) => this.write(ptr),
    };
  }
}

export class FileTest {
  private exports_: CommonTestExports | null = null;
  private readonly promises_: Record<number, (ptr: number) => void> = {};

  constructor() {}

  get exports(): CommonTestExports | null {
    return this.exports_;
  }
  set exports(value: CommonTestExports | null) {
    this.exports_ = value;
  }

  async waitFor(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  heapItem(ptr: number): Uint32Array {
    if (!this.exports_) {
      throw new Error("FileTest object not initialized");
    }
    return new Uint32Array(this.exports_.memory.buffer.slice(ptr, ptr + 12));
  }

  addImportPromise(ptr: number): Promise<number> {
    return new Promise((resolve) => (this.promises_[ptr] = resolve));
  }

  isImportPromise(ptr: number) {
    const heapWords = this.heapItem(ptr);
    if ((heapWords[0] & SchemeType.Mask) != SchemeType.Cont) {
      return false;
    }

    const continuation = this.heapItem(heapWords[1]);
    return continuation[0] == 204;
  }

  pokeMemory(ptr: number, data: ArrayBuffer) {
    if (!this.exports_) {
      throw new Error("FileTest object not initialized");
    }
    pokeMemory(this.exports_, ptr, data);
  }

  settleImportPromise(promise: number, value: number): boolean {
    for (const ptrStr of Object.keys(this.promises_)) {
      const ptr = Number(ptrStr);
      if (isNaN(ptr)) {
        continue;
      }
      if (!this.isImportPromise(ptr)) {
        console.error("Expecting Import promise");
      }

      const heapWords = this.heapItem(ptr);
      const continuation = this.heapItem(heapWords[1]);
      if (continuation[2] === promise) {
        continuation[0] = 0;
        continuation[2] = value;
        this.pokeMemory(heapWords[1], continuation.buffer);

        const resolver = this.promises_[ptr];
        delete this.promises_[ptr];
        resolver(ptr);
        return true;
      }
    }
    return false;
  }

  doRead(filenamePtr: number): number {
    const word = new Uint32Array(crypto.randomBytes(8).buffer);
    const promise = word[0];

    this.doReadImpl(promise, filenamePtr).catch((err) => console.error(err));

    return promise;
  }

  async doReadImpl(promise: number, filenamePtr: number): Promise<void> {
    if (!this.exports_) {
      throw new Error("FileTest object not initialized");
    }

    try {
      const filename = getString(this.exports_, filenamePtr);
      const fullpath = path.resolve(__dirname, "../src/scheme", filename);
      const response = await fs.readFile(fullpath, "utf-8");
      while (
        !this.settleImportPromise(
          promise,
          createHeapString(this.exports_, response)
        )
      ) {
        await this.waitFor(100);
      }
    } catch (err) {
      console.error(err);
      this.settleImportPromise(
        promise,
        createHeapError(
          this.exports_,
          "file-read",
          err instanceof Error ? err.message : (err as any).toString()
        )
      );
    }
  }

  read(filenamePtr: number): number {
    return this.doRead(filenamePtr);
  }

  get module(): FileModule {
    return {
      read: (filenamePtr: number) => this.read(filenamePtr),
    };
  }
}
