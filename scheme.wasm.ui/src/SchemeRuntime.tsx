type WriteCallback = (str: string) => void;

export class SchemeRuntime {
  private instance_: WebAssembly.Instance;
  private exports_: WebAssembly.Exports;
  private readonly unicodeData_: Record<number, ArrayBuffer> = {};
  private readonly lines_: string[] = [];
  private readonly writeCallbacks_: WriteCallback[] = [];
  private started_: boolean = false;
  private env_: number = 0;

  constructor(instance: WebAssembly.Instance) {
    this.instance_ = instance;
    this.exports_ = instance.exports;
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
    return this.instance_;
  }

  get exports(): WebAssembly.Exports {
    return this.exports_;
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
    const line = this.lines_.shift() || "";
    return this.createString(line);
  }

  writer(ptr: number) {
    const str = this.getString(ptr);
    this.writeCallbacks_.forEach((el) => el(str));
  }

  exit(exitCode: number) {}

  unicodeLoadData(block: number, ptr: number) {
    const src = new Uint8Array(this.unicodeData_[block]);
    const dst = new Uint8Array(this.memory.buffer);
    dst.set(src, ptr);
  }

  processLine(str: string): string {
    const written: string[] = [];
    const onWrite = (str: string) => {
      written.push(str);
    };
    this.addEventListener("write", onWrite);
    this.lines_.push(str);
    if (!this.started_) {
      this.env_ = this.environmentInit(this.gHeap, 0);
      this.registerBuiltins(this.gHeap, this.env_);
      this.started_ = true;
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
  }

  async loadUnicodeBlocks() {
    const response = await fetch("./unicode/blocks.json");
    const data = await response.json();
    if (!Reflect.has(data, "block" || Array.isArray(data.block))) {
      throw new Error("Invalid unicode block list");
    }
    for (const blockIndex of data.block) {
      if (typeof blockIndex !== "number") {
        continue;
      }
      const url = `./unicode/${blockIndex.toString(16).padStart(4, "0")}.bin`;
      const blockResponse = await fetch(url);
      this.unicodeData_[blockIndex] = await blockResponse.arrayBuffer();
    }
  }

  static async load() {
    const response = await fetch("./wasm/scheme.wasm");

    let reader = () => 0;
    let writer = (ptr: number) => {};
    let exit = (exitCode: number) => {};
    let unicodeLoadData = (block: number, ptr: number) => {};

    const imports: WebAssembly.Imports = {
      io: {
        read: () => reader(),
        write: (ptr: number) => writer(ptr),
      },
      process: {
        exit: (exitCode: number) => exit(exitCode),
      },
      unicode: {
        loadData: (block: number, ptr: number) => unicodeLoadData(block, ptr),
      },
    };

    const module = await WebAssembly.instantiateStreaming(response, imports);
    const runtime = new SchemeRuntime(module.instance);
    await runtime.loadUnicodeBlocks();
    reader = () => runtime.reader();
    writer = (ptr: number) => runtime.writer(ptr);
    exit = (exitCode: number) => runtime.exit(exitCode);
    unicodeLoadData = (block: number, ptr: number) =>
      runtime.unicodeLoadData(block, ptr);
    return runtime;
  }
}
