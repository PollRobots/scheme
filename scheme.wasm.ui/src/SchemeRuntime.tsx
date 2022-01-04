import pako from "pako";

type WriteCallback = (str: string) => void;

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

class RuntimeExit extends Error {}

export class SchemeRuntime {
  private readonly module_: WebAssembly.Module;
  private instance_: WebAssembly.Instance | undefined;
  private exports_: WebAssembly.Exports | undefined;
  private readonly unicodeData_: Record<number, ArrayBuffer> = {};
  private readonly lines_: string[] = [];
  private readonly writePriorityCallbacks_: WriteCallback[] = [];
  private readonly writeCallbacks_: WriteCallback[] = [];
  private initialized_: boolean = false;
  private env_: number = 0;
  private partial_: boolean = false;
  private waiting_: boolean = false;
  private readonly promises_: number[] = [];

  constructor(module: WebAssembly.Module) {
    this.module_ = module;
  }

  addEventListener(event: "write" | "write-priority", callback: WriteCallback) {
    if (event === "write") {
      this.writeCallbacks_.push(callback);
    } else {
      this.writePriorityCallbacks_.push(callback);
    }
  }

  removeEventListener(
    event: "write" | "write-priority",
    callback: WriteCallback
  ) {
    if (event === "write") {
      const index = this.writeCallbacks_.findIndex((el) => el === callback);
      if (index >= 0) {
        this.writeCallbacks_.splice(index, 1);
      }
    } else {
      const index = this.writePriorityCallbacks_.findIndex(
        (el) => el === callback
      );
      if (index >= 0) {
        this.writePriorityCallbacks_.splice(index, 1);
      }
    }
  }

  get instance(): WebAssembly.Instance {
    if (!this.instance_) {
      throw new Error("Invalid operation");
    }
    return this.instance_;
  }

  get exports(): WebAssembly.Exports {
    if (!this.exports_) {
      throw new Error("Invalid operation");
    }
    return this.exports_;
  }

  get replEnv(): number {
    return this.env_;
  }

  get stopped(): boolean {
    return this.instance_ === undefined;
  }

  get partial(): boolean {
    return this.partial_;
  }

  get waiting(): boolean {
    return this.waiting_;
  }

  get memory(): WebAssembly.Memory {
    return this.exports.memory as WebAssembly.Memory;
  }

  runtimeInit() {
    (this.exports.runtimeInit as () => void)();
  }

  mallocInit() {
    (this.exports.mallocInit as () => void)();
  }

  runtimeCleanup() {
    (this.exports.runtimeCleanup as () => void)();
  }

  get gHeap(): number {
    return (this.exports.gHeap as WebAssembly.Global).value as number;
  }

  get gReader(): number {
    return (this.exports.gReader as WebAssembly.Global).value as number;
  }

  get gGcIsCollecting(): boolean {
    return !!((this.exports.gGcIsCollecting as WebAssembly.Global)
      .value as number);
  }

  get gGcCollectionCount(): number {
    return (this.exports.gGcCollectionCount as WebAssembly.Global)
      .value as number;
  }

  get gGcCollectedCount(): number {
    return (this.exports.gGcCollectedCount as WebAssembly.Global)
      .value as number;
  }

  get gGcTotalCollectedCount(): number {
    return (this.exports.gGcTotalCollectedCount as WebAssembly.Global)
      .value as number;
  }

  get gGcNotCollectedCount(): number {
    return (this.exports.gGcNotCollectedCount as WebAssembly.Global)
      .value as number;
  }

  get gGcTotalNotCollectedCount(): number {
    return (this.exports.gGcTotalNotCollectedCount as WebAssembly.Global)
      .value as number;
  }

  strFromCodePoints(ptr: number, len: number): number {
    return (
      this.exports.strFromCodePoints as (ptr: number, len: number) => number
    )(ptr, len);
  }

  heapAllocString(str: string): number {
    return (this.exports.heapAllocString as (ptr: number) => number)(
      this.createString(str)
    );
  }

  heapAllocError(symbol: string, message: string): number {
    return (
      this.exports.heapAllocError as (symbol: number, message: number) => number
    )(this.createString(symbol), this.createString(message));
  }

  readerRollback(reader: number) {
    (this.exports.readerRollback as (reader: number) => void)(reader);
  }

  read(): number {
    return (this.exports.read as () => number)();
  }

  environmentInit(heap: number, outer: number): number {
    return (
      this.exports.environmentInit as (heap: number, outer: number) => number
    )(heap, outer);
  }

  registerBuiltins(heap: number, env: number) {
    (this.exports.registerBuiltins as (heap: number, env: number) => void)(
      heap,
      env
    );
  }

  print(ptr: number) {
    (this.exports.print as (ptr: number) => void)(ptr);
  }

  eval(env: number, ptr: number): number {
    return (this.exports.eval as (env: number, ptr: number) => number)(
      env,
      ptr
    );
  }

  gcRun(env: number) {
    (this.exports.gcRun as (env: number) => void)(env);
  }

  malloc(size: number): number {
    return (this.exports.malloc as (size: number) => number)(size);
  }

  free(ptr: number) {
    (this.exports.free as (ptr: number) => void)(ptr);
  }

  heapItem(ptr: number): Uint32Array {
    return new Uint32Array(this.memory.buffer.slice(ptr, ptr + 12));
  }

  pokeMemory(ptr: number, data: ArrayBuffer) {
    const byteArray = new Uint8Array(data);
    const view = new Uint8Array(this.memory.buffer);
    for (let i = 0; i < byteArray.length; i++) {
      view[i + ptr] = byteArray[i];
    }
  }

  isError(ptr: number) {
    return (this.heapItem(ptr)[0] & SchemeType.Mask) == SchemeType.Error;
  }

  isEofError(ptr: number) {
    const heapWords = this.heapItem(ptr);
    if ((heapWords[0] & SchemeType.Mask) != SchemeType.Error) {
      return false;
    }
    const symbol = this.heapItem(heapWords[1]);
    return this.getString(symbol[1]) == "eof";
  }

  isImportPromise(ptr: number) {
    const heapWords = this.heapItem(ptr);
    if ((heapWords[0] & SchemeType.Mask) != SchemeType.Cont) {
      return false;
    }
    // continuation is conveniently the same size as a heap item
    const continuation = this.heapItem(heapWords[1]);
    // check the continuation function
    // %cont-import-promise is defined as 204 in builtins.wat
    return continuation[0] == 204;
  }

  addImportPromise(ptr: number) {
    this.promises_.push(ptr);
  }

  resolveImportPromise(promise: number, text: string): boolean {
    return this.settleImportPromise(promise, this.heapAllocString(text));
  }

  rejectImportPromise(
    promise: number,
    symbol: string,
    message: string
  ): boolean {
    const value = this.heapAllocError(symbol, message);

    return this.settleImportPromise(promise, value);
  }

  settleImportPromise(promise: number, value: number): boolean {
    for (let index = 0; index != this.promises_.length; index++) {
      const ptr = this.promises_[index];
      const heapWords = this.heapItem(ptr);
      if ((heapWords[0] & SchemeType.Mask) != SchemeType.Cont) {
        console.error(
          "Unexpected object in import promise list: ",
          ptr,
          heapWords
        );
        continue;
      }

      const continuation = this.heapItem(heapWords[1]);
      if (continuation[0] != 204) {
        console.error("Unexpected function in continuation", ptr, continuation);
        continue;
      }

      if (continuation[2] == promise) {
        // this is the promise.
        continuation[0] = 0;
        continuation[2] = value;
        this.pokeMemory(heapWords[1], continuation.buffer);
        this.promises_.splice(index, 1);

        try {
          this.evalInput(ptr);
        } catch (err) {
          console.error(err);
          this.instance_ = undefined;
          this.exports_ = undefined;
          this.initialized_ = false;
          if (err instanceof Error && typeof err.stack == "string") {
            err.stack
              .split("\n")
              .forEach((el) => this.onWrite(`\x1B[0;91m${el}\n`));
            this.onWrite("\n");
          } else {
            this.onWrite(`\x1B[0;31m${err}\n`);
          }
          this.onWrite(
            "\n\x1B[0;94mPress <Enter> to restart scheme runtime.\x1B[0m\n"
          );
        }

        if (this.promises_.length == 0) {
          this.waiting_ = false;
        }
        return true;
      }
    }
    return false;
  }

  isNil(ptr: number) {
    return (this.heapItem(ptr)[0] & SchemeType.Mask) == SchemeType.Nil;
  }

  createString(str: string): number {
    const codePoints = new Uint32Array(
      Array.from(str).map((el) => el.codePointAt(0) || 0xfffd)
    );
    const byteArray = new Uint8Array(codePoints.buffer);
    const ptr = this.malloc(codePoints.length * 4);
    const view = new Uint8Array(this.memory.buffer);
    for (let i = 0; i < byteArray.length; i++) {
      view[i + ptr] = byteArray[i];
    }
    const strPtr = this.strFromCodePoints(ptr, codePoints.length);
    this.free(ptr);
    return strPtr;
  }

  private getString(ptr: number) {
    const len = new Uint32Array(this.memory.buffer.slice(ptr, ptr + 4))[0];
    const utf8 = new Uint8Array(
      this.memory.buffer.slice(ptr + 4, ptr + len + 4)
    );
    const str = new TextDecoder().decode(utf8);
    return str;
  }

  reader(): number {
    if (this.lines_.length == 0) {
      return 0;
    }
    const line = this.lines_.shift() || "";
    return this.createString(line);
  }

  writer(ptr: number) {
    const str = this.getString(ptr);
    this.onWrite(str);
  }

  onWrite(str: string) {
    if (this.writePriorityCallbacks_.length) {
      this.writePriorityCallbacks_.forEach((el) => el(str));
    } else {
      this.writeCallbacks_.forEach((el) => el(str));
    }
  }

  exit(exitCode: number) {
    throw new RuntimeExit(`Scheme exited with code: ${exitCode}`);
  }

  unicodeLoadData(block: number, ptr: number) {
    const src = new Uint8Array(this.unicodeData_[block]);
    const dst = new Uint8Array(this.memory.buffer);
    dst.set(src, ptr);
  }

  waitFor(timeoutMs: number): Promise<void> {
    return new Promise<void>((resolve) => {
      window.setTimeout(() => resolve(), timeoutMs);
    });
  }

  async doFileRead(filenamePtr: number): Promise<void> {
    try {
      const filename = this.getString(filenamePtr);
      const response = await fetch(`./scheme/${filename}`);
      if (!response.ok) {
        throw new Error(response.statusText);
      }
      const text = await response.text();
      while (!this.resolveImportPromise(filenamePtr, text)) {
        await this.waitFor(100);
      }
    } catch (err) {
      this.rejectImportPromise(
        filenamePtr,
        "file-read",
        err instanceof Error ? err.message : "unknown"
      );
      console.error(err);
    }
  }

  fileRead(filenamePtr: number) {
    this.doFileRead(filenamePtr);
    return filenamePtr;
  }

  processLine(str: string): void {
    if (this.stopped) {
      return;
    }
    if (this.waiting_) {
      return;
    }
    try {
      if (!this.initialized_) {
        this.env_ = this.environmentInit(this.gHeap, 0);
        this.registerBuiltins(this.gHeap, this.env_);
        this.initialized_ = true;
        this.lines_.push('(include "prelude.scm")\n');
      }
      this.lines_.push(str);
      while (this.lines_.length) {
        const input = this.read();
        if (this.isEofError(input)) {
          this.partial_ = true;
          break;
        } else if (input != 0) {
          this.evalInput(input);
          this.partial_ = false;
        }
      }
    } catch (err) {
      console.error(err);
      this.instance_ = undefined;
      this.exports_ = undefined;
      this.initialized_ = false;
      if (err instanceof RuntimeExit) {
        this.onWrite(`\x1B[0;32m${err.message}\n`);
      } else {
        if (err instanceof Error && typeof err.stack == "string") {
          err.stack
            .split("\n")
            .forEach((el) => this.onWrite(`\x1B[0;31m${el}\n`));
          this.onWrite("\n");
        } else {
          this.onWrite(`\x1B[0;31m${err}\n`);
        }
      }
      this.onWrite(
        "\n\x1B[0;94mPress <Enter> to restart scheme runtime.\x1B[0m\n"
      );
    }
  }

  evalInput(input: number) {
    const result = this.eval(this.env_, input);
    if (this.isImportPromise(result)) {
      this.waiting_ = true;
      this.onWrite("\x1B[0;94mWaiting...\x1B[0m\n");
      this.addImportPromise(result);
    } else if (!this.isNil(result)) {
      this.print(result);
    }
  }

  async loadUnicodeBlocks() {
    const response = await fetch("./unicode/blocks.json.gz");
    const compressed = await response.arrayBuffer();
    const raw = pako.inflate(new Uint8Array(compressed), { to: "string" });
    const data = JSON.parse(raw);
    if (typeof data !== "object") {
      throw new Error("Invalid unicode block data");
    }
    for (const key of Object.keys(data)) {
      const blockIndex = Number(key);
      if (isNaN(blockIndex)) {
        continue;
      }
      const blockData = Uint8Array.from(atob(data[key]), (c) =>
        c.charCodeAt(0)
      );
      this.unicodeData_[blockIndex] = blockData;
    }
  }

  async start() {
    const imports: WebAssembly.Imports = {
      io: {
        read: () => this.reader(),
        write: (ptr: number) => this.writer(ptr),
      },
      process: {
        exit: (exitCode: number) => this.exit(exitCode),
      },
      unicode: {
        loadData: (block: number, ptr: number) =>
          this.unicodeLoadData(block, ptr),
      },
      file: {
        read: (filenamePtr: number) => this.fileRead(filenamePtr),
      },
    };

    this.instance_ = await WebAssembly.instantiate(this.module_, imports);
    this.exports_ = this.instance.exports;
    this.initialized_ = false;
  }

  static async load() {
    const module = await WebAssembly.compileStreaming(
      fetch("./wasm/scheme.wasm")
    );
    const runtime = new SchemeRuntime(module);
    await runtime.loadUnicodeBlocks();
    await runtime.start();
    return runtime;
  }
}
