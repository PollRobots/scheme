#!/usr/bin/env node
import { use } from "chai";
import fs from "fs";

interface CoreExports {
  memory: WebAssembly.Memory;
  gHeap: () => number;
  gReader: () => number;
  gGcIsCollecting: () => number;
  gGcCollectionCount: () => number;
  gGcCollectedCount: () => number;
  gGcNotCollectedCount: () => number;
  gGcTotalCollectedCount: () => number;
  gGcTotalNotCollectedCount: () => number;
  strFromCodePoints: (ptr: number, len: number) => number;
  readerRollback: (reader: number) => void;
  registerBuiltins: (heap: number, env: number) => void;
  mallocInit: () => void;
  runtimeInit: () => void;
  runtimeCleanup: () => void;
  read: () => number;
  environmentInit: (heap: number, outer: number) => number;
  print: (ptr: number) => void;
  eval: (env: number, ptr: number) => number;
  gcRun: (env: number) => void;
  malloc: (size: number) => number;
  free: (ptr: number) => void;
}

class SchemeRuntime {
  private instance: WebAssembly.Instance;
  private exports: WebAssembly.Exports;
  private readonly readlineBuffer: Buffer = Buffer.alloc(4096);
  private rlbOffset: number = 0;

  constructor(instance: WebAssembly.Instance) {
    this.instance = instance;
    this.exports = this.instance.exports;
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

  readline(): string {
    while (true) {
      const bytesRead = fs.readSync(
        process.stdin.fd,
        this.readlineBuffer,
        this.rlbOffset,
        this.readlineBuffer.length - this.rlbOffset,
        null
      );

      this.rlbOffset += bytesRead;

      for (let i = 0; i < this.rlbOffset; i++) {
        if (this.readlineBuffer[i] == 0x0a) {
          const rawline = this.readlineBuffer.slice(0, i);
          const line = rawline.toString("utf-8");
          i++;
          let j: number;
          for (j = 0; i < this.rlbOffset; i++, j++) {
            this.readlineBuffer[j] = this.readlineBuffer[i];
          }
          this.rlbOffset = j;
          return line;
        }
      }
    }
  }

  private ioRead(): number {
    const line = this.readline();
    return this.createString(line);
  }

  private ioWrite(ptr: number) {
    const len = new Uint32Array(this.memory.buffer.slice(ptr, ptr + 4))[0];
    const utf8 = new Uint8Array(
      this.memory.buffer.slice(ptr + 4, ptr + len + 4)
    );
    const str = new TextDecoder().decode(utf8);
    fs.writeSync(process.stdout.fd, str);
  }

  runRepl() {
    const env = this.environmentInit(this.gHeap, 0);
    this.registerBuiltins(this.gHeap, env);
    const prompt = "\n> ";
    let usePrompt = true;

    while (true) {
      if (usePrompt) {
        fs.writeSync(process.stdout.fd, prompt);
        usePrompt = false;
      }
      const input = this.read();
      if (this.isError(input)) {
        this.readerRollback(this.gReader);
        continue;
      }
      usePrompt = true;
      const result = this.eval(env, input);
      if (!this.isNil(result)) {
        this.print(result);
      }
    }
  }

  public static async create(): Promise<SchemeRuntime> {
    console.log("Loading runtime...")
    const wasm = fs.readFileSync("dist/scheme.wasm");

    let reader = () => 0;
    let writer = (ptr: number) => {};
    try {
      const imports: WebAssembly.Imports = {};
      imports["io"] = {
        read: () => reader(),
        write: (ptr: number) => writer(ptr),
      };
      console.log("Instatiating runtime...")
      const module = await WebAssembly.instantiate(wasm, imports);
      const runtime = new SchemeRuntime(module.instance);
      reader = () => runtime.ioRead();
      writer = (ptr: number) => runtime.ioWrite(ptr);
      return runtime;
    } catch (err) {
      console.error(err);
      process.exit(1);
    }
  }
}

async function main() {
  console.log("Scheme.wasm")
  console.log("===========")
  const runtime = await SchemeRuntime.create();
  runtime.runRepl();
}

main().catch((err) => console.error(err));
