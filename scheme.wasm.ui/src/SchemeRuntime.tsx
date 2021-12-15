import pako from "pako";

type WriteCallback = (str: string) => void;

export class SchemeRuntime {
  private readonly module_: WebAssembly.Module;
  private instance_: WebAssembly.Instance | undefined;
  private exports_: WebAssembly.Exports | undefined;
  private readonly unicodeData_: Record<number, ArrayBuffer> = {};
  private readonly lines_: string[] = [];
  private readonly writeCallbacks_: WriteCallback[] = [];
  private initialized_: boolean = false;
  private env_: number = 0;

  constructor(module: WebAssembly.Module) {
    this.module_ = module;
  }

  addEventListener(event: "write", callback: WriteCallback) {
    this.writeCallbacks_.push(callback);
  }

  removeEventListener(event: "write", callback: WriteCallback) {
    const index = this.writeCallbacks_.findIndex((el) => el === callback);
    if (index >= 0) {
      this.writeCallbacks_.splice(index, 1);
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

  get stopped(): boolean {
    return this.instance_ === undefined;
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

  isError(ptr: number) {
    return (this.heapItem(ptr)[0] & 0xf) == 13;
  }

  isEofError(ptr: number) {
    const heapWords = this.heapItem(ptr);
    if ((heapWords[0] & 0xf) != 13) {
      return false;
    }
    return this.getString(heapWords[1]) == "eof";
  }

  isNil(ptr: number) {
    return (this.heapItem(ptr)[0] & 0xf) == 1;
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
    this.writeCallbacks_.forEach((el) => el(str));
  }

  exit(exitCode: number) {
    throw new Error(`Scheme exited with code: ${exitCode}`);
  }

  unicodeLoadData(block: number, ptr: number) {
    const src = new Uint8Array(this.unicodeData_[block]);
    const dst = new Uint8Array(this.memory.buffer);
    dst.set(src, ptr);
  }

  processLine(str: string): string {
    if (this.stopped) {
      return "";
    }
    try {
      const written: string[] = [];
      const onWrite = (str: string) => {
        written.push(str);
      };
      this.addEventListener("write", onWrite);
      this.lines_.push(str);
      if (!this.initialized_) {
        this.env_ = this.environmentInit(this.gHeap, 0);
        this.registerBuiltins(this.gHeap, this.env_);
        this.initialized_ = true;
      }

      const input = this.read();
      if (this.isEofError(input)) {
        this.readerRollback(this.gReader);
      } else {
        const result = this.eval(this.env_, input);
        if (!this.isNil(result)) {
          this.print(result);
        }
      }
      this.removeEventListener("write", onWrite);
      return written.join("");
    } catch (err) {
      console.error(err);
      this.instance_ = undefined;
      this.exports_ = undefined;
      this.initialized_ = false;
      return `\x1B[0;31m${err}

\x1B[0;94mPress <Enter> to restart scheme runtime.\x1B[0m
`;
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
