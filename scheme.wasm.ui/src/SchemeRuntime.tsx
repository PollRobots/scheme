import pako from "pako";
import { Jiffies } from "./Jiffies";
import { SchemeType } from "./SchemeType";

type WriteCallback = (str: string) => void;
type DebugCallback = (
  ptr: number,
  resolver: (resultStep: boolean) => void
) => void;

class RuntimeExit extends Error {}

interface ImportPromiseResolver {
  resolver: (ptr: number) => void;
  promise: number;
}

export class SchemeRuntime {
  private readonly module_: WebAssembly.Module;
  private instance_: WebAssembly.Instance | undefined;
  private exports_: WebAssembly.Exports | undefined;
  private readonly unicodeData_: Record<number, ArrayBuffer> = {};
  private readonly lines_: string[] = [];
  private readonly writePriorityCallbacks_: WriteCallback[] = [];
  private readonly writeCallbacks_: WriteCallback[] = [];
  private readonly debugCallbacks_: DebugCallback[] = [];
  private initialized_: boolean = false;
  private env_: number = 0;
  private partial_: boolean = false;
  private waiting_: boolean = false;
  private readonly promises_: Map<number, ImportPromiseResolver> = new Map();
  private readonly jiffies_: Jiffies = Jiffies.init();
  private readonly environment_: Map<string, string> = new Map();

  constructor(module: WebAssembly.Module) {
    this.module_ = module;
  }

  addEventListener(event: "write", callback: WriteCallback): void;
  addEventListener(event: "write-priority", callback: WriteCallback): void;
  addEventListener(event: "debug", callback: DebugCallback): void;

  addEventListener(event: unknown, callback: unknown) {
    if (event === "write") {
      this.writeCallbacks_.push(callback as WriteCallback);
    } else if (event === "write-priority") {
      this.writePriorityCallbacks_.push(callback as WriteCallback);
    } else if (event === "debug") {
      this.debugCallbacks_.push(callback as DebugCallback);
    }
  }

  removeEventListener(event: "write", callback: WriteCallback): void;
  removeEventListener(event: "write-priority", callback: WriteCallback): void;
  removeEventListener(event: "debug", callback: DebugCallback): void;

  removeEventListener(event: unknown, callback: unknown) {
    if (event === "write") {
      const index = this.writeCallbacks_.findIndex((el) => el === callback);
      if (index >= 0) {
        this.writeCallbacks_.splice(index, 1);
      }
    } else if (event === "write-priority") {
      const index = this.writePriorityCallbacks_.findIndex(
        (el) => el === callback
      );
      if (index >= 0) {
        this.writePriorityCallbacks_.splice(index, 1);
      }
    } else if (event === "debug") {
      const index = this.debugCallbacks_.findIndex((el) => el === callback);
      if (index >= 0) {
        this.debugCallbacks_.splice(index, 1);
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

  get gNil(): number {
    return (this.exports.gNil as WebAssembly.Global).value as number;
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

  get gDebug(): boolean {
    return ((this.exports.gDebug as WebAssembly.Global).value as number) != 0;
  }

  set gDebug(value: boolean) {
    (this.exports.gDebug as WebAssembly.Global).value = value ? 1 : 0;
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

  heapAllocCons(car: number, cdr: number): number {
    return (this.exports.heapAllocCons as (car: number, cdr: number) => number)(
      car,
      cdr
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

  heapItem(ptr: number, size: number = 3): Uint32Array {
    return new Uint32Array(this.memory.buffer.slice(ptr, ptr + size * 4));
  }

  pokeMemory(ptr: number, data: ArrayBuffer) {
    const byteArray = new Uint8Array(data);
    const view = new Uint8Array(this.memory.buffer);
    view.set(byteArray, ptr);
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

  isDebugBreak(ptr: number) {
    const heapWords = this.heapItem(ptr);
    if ((heapWords[0] & SchemeType.Mask) != SchemeType.Cont) {
      return false;
    }
    // continuation is conveniently the same size as a heap item
    const continuation = this.heapItem(heapWords[1]);
    // check the continuation function
    // %debug-fn is defined as -2 in builtins.wat
    return continuation[0] == 0xffff_fffe; //-2;
  }

  async addDebugPromise(ptr: number): Promise<number> {
    const heapCons = this.heapItem(ptr);
    if (!this.debugCallbacks_.length) {
      return heapCons[2];
    }
    const p = new Promise<boolean>((resolve) => {
      try {
        if (this.debugCallbacks_.length == 0) {
          resolve(false);
        } else {
          this.debugCallbacks_[0](ptr, resolve);
        }
      } catch (err) {
        console.error(err);
        resolve(false);
      }
    });
    const evalFramePtr = heapCons[2];
    const evalFrame = this.heapItem(evalFramePtr);
    const argsFramePtr = evalFrame[2];
    const argsFrame = this.heapItem(argsFramePtr);

    const heapCont = this.heapItem(heapCons[1]);
    if (heapCont[1] == 0 && heapCont[2] == 0) {
      if (await p) {
        // move the initial cons, two items down the continuation stack
        heapCons[2] = argsFrame[2];
        this.pokeMemory(ptr, heapCons.buffer);
        argsFrame[2] = ptr;
        this.pokeMemory(argsFramePtr, argsFrame.buffer);

        //use the expression environtment
        heapCont[1] = this.heapItem(evalFrame[1])[1];
        // put a g-nil into the continuation frame object in the argument
        // position (this allows the result to be prepended)
        heapCont[2] = this.gNil;
        this.pokeMemory(heapCons[1], heapCont.buffer);
      }
      return evalFramePtr;
    } else {
      await p;
      heapCont[0] = -3; // skip value
      this.pokeMemory(heapCons[1], heapCont.buffer);
      return ptr;
    }
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

  addImportPromise(ptr: number): Promise<number> {
    const heapWords = this.heapItem(ptr);
    const continuation = this.heapItem(heapWords[1]);
    const index = continuation[2];
    return new Promise<number>((resolve) =>
      this.promises_.set(index, { resolver: resolve, promise: ptr })
    );
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

  settleImportPromise(promiseIdx: number, value: number): boolean {
    const promiseObj = this.promises_.get(promiseIdx);
    if (!promiseObj) {
      return false;
    }
    const { resolver, promise: ptr } = promiseObj;

    const heapWords = this.heapItem(ptr);
    if ((heapWords[0] & SchemeType.Mask) != SchemeType.Cont) {
      throw new Error("Unexpected object in import promise list");
    }

    const continuation = this.heapItem(heapWords[1]);
    if (continuation[0] != 204) {
      throw new Error("Unexpected function in continuation");
    }

    if (continuation[2] !== promiseIdx) {
      throw new Error("Unexpected promise in continuation");
    }

    // this is the promise.
    continuation[0] = 0;
    continuation[2] = value;
    this.pokeMemory(heapWords[1], continuation.buffer);
    this.promises_.delete(promiseIdx);

    this.waiting_ = this.promises_.size > 0;
    resolver(ptr);
    return true;
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
    view.set(byteArray, ptr);
    const strPtr = this.strFromCodePoints(ptr, codePoints.length);
    this.free(ptr);
    return strPtr;
  }

  getString(ptr: number) {
    const len = new Uint32Array(this.memory.buffer.slice(ptr, ptr + 4))[0];
    const utf8 = new Uint8Array(len);
    utf8.set(new Uint8Array(this.memory.buffer.slice(ptr + 4, ptr + len + 4)));
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

  getEnvironmentVariable(name: number): number {
    const value = this.environment_.get(this.getString(name));
    if (typeof value === "string") {
      return this.heapAllocString(value);
    }
    return 0;
  }

  getEnvironmentVariables(): number {
    let tail = this.gNil;
    for (const entry of this.environment_.entries()) {
      const pair = this.heapAllocCons(
        this.heapAllocString(entry[0]),
        this.heapAllocString(entry[1])
      );
      tail = this.heapAllocCons(pair, tail);
    }
    return tail;
  }

  setEnvironmentVariable(name: number, value: number) {
    this.environment_.set(this.getString(name), this.getString(value));
  }

  commandLine() {
    return this.heapAllocCons(
      this.heapAllocString("/usr/local/bin/scheme.wasm"),
      this.gNil
    );
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

  async doFileRead(promiseIdx: number, filenamePtr: number): Promise<void> {
    try {
      const filename = this.getString(filenamePtr);
      const response = await fetch(`./scheme/${filename}`);
      if (!response.ok) {
        throw new Error(response.statusText);
      }
      const text = await response.text();
      while (!this.resolveImportPromise(promiseIdx, text)) {
        await this.waitFor(100);
      }
    } catch (err) {
      this.rejectImportPromise(
        promiseIdx,
        "file-read",
        err instanceof Error ? err.message : "unknown"
      );
      console.error(err);
    }
  }

  doRead(filenamePtr: number): number {
    const word = new Uint32Array(1);
    self.crypto.getRandomValues(word);
    const promiseIdx = word[0];

    this.doFileRead(promiseIdx, filenamePtr).catch(console.error);

    return promiseIdx;
  }

  fileRead(filenamePtr: number) {
    return this.doRead(filenamePtr);
  }

  async processLine(str: string): Promise<void> {
    if (this.stopped) {
      return;
    }
    if (this.waiting_) {
      return;
    }
    try {
      if (!this.initialized_) {
        this.environment_.clear();
        this.environment_.set("HOME", "/home/schemer");
        this.environment_.set("LOGNAME", "schemer");
        this.environment_.set("PATH", "/usr/local/bin:/usr/bin");
        this.environment_.set("PWD", "/home/schemer");
        this.environment_.set("USER", "schemer");
        this.env_ = this.environmentInit(this.gHeap, 0);
        this.registerBuiltins(this.gHeap, this.env_);
        this.initialized_ = true;
        this.lines_.push('(include "prelude.scm")\n');
      }
      this.lines_.push(str);
      while (this.lines_.length) {
        let expr = this.read();
        let first = true;
        if (this.isEofError(expr)) {
          this.partial_ = true;
          break;
        } else if (expr != 0) {
          while (true) {
            const result = this.eval(this.env_, expr);
            if (this.isDebugBreak(result)) {
              expr = await this.addDebugPromise(result);
              continue;
            } else if (this.isImportPromise(result)) {
              expr = await this.addImportPromise(result);
              continue;
            } else if (result != 0 && !this.isNil(result)) {
              if (first) {
                first = false;
              } else {
                this.onWrite("\n");
              }
              this.print(result);
            }
            break;
          }
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
        getEnvironmentVariable: (name: number) =>
          this.getEnvironmentVariable(name),
        getEnvironmentVariables: () => this.getEnvironmentVariables(),
        setEnvironmentVariable: (name: number, value: number) =>
          this.setEnvironmentVariable(name, value),
        commandLine: () => this.commandLine(),
      },
      unicode: {
        loadData: (block: number, ptr: number) =>
          this.unicodeLoadData(block, ptr),
      },
      file: {
        read: (filenamePtr: number) => this.fileRead(filenamePtr),
      },
      time: {
        currentSecond: () => Date.now() / 1000,
        currentJiffy: () => this.jiffies_.current,
        jiffiesPerSecond: () => this.jiffies_.jiffiesPerSecond,
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
