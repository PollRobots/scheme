import * as messages from "./worker/messages";

type StatusEventHandler = () => void;
type WriteCallback = (msg: string) => void;
export interface DebugBreakEvent {
  ptr: number;
  env: number;
  expr: string;
  step: (resultStep: boolean) => void;
}

type DebugBreakHander = (evt: DebugBreakEvent) => void;
type ResponseResolver = (msg: messages.WorkerMessage) => void;

declare global {
  interface Window {
    kWorkerScript: string;
  }
}

type SendableMessages =
  | messages.DebugStep
  | messages.EnableDebug
  | messages.EnvRequest
  | messages.GcRequest
  | messages.HeapRequest
  | messages.InputMessage
  | messages.PrintRequest
  | messages.StartMessage;

export class RuntimeWorker {
  private loaded_: boolean = false;
  private worker_?: Worker;
  private memory_?: ArrayBuffer;
  private heap_?: number;
  private idCounter_: number = 1;
  private status_: "running" | "waiting" | "stopped" | "partial" | "none" =
    "none";
  private memorySize_: number = 0;
  private debugging_: boolean = false;
  private readonly statusListeners: StatusEventHandler[] = [];
  private readonly writeListeners: WriteCallback[] = [];
  private readonly debugListeners: DebugBreakHander[] = [];
  private readonly pendingResponses: Map<number, ResponseResolver> = new Map();
  private readonly pendingOutput: string[] = [];
  private outputCounter = 0;

  constructor() {}

  get loaded(): boolean {
    return this.loaded_;
  }

  get running(): boolean {
    return this.status_ === "running";
  }

  get waiting(): boolean {
    return this.status_ === "waiting";
  }

  get stopped(): boolean {
    return this.status_ === "stopped";
  }

  get partial(): boolean {
    return this.status_ === "partial";
  }

  get memorySize(): number {
    return this.memorySize_;
  }

  get debugging(): boolean {
    return this.debugging_;
  }

  get heap(): number {
    return this.heap_ || 0;
  }

  get memory(): { buffer: ArrayBuffer } {
    return this.memory_
      ? { buffer: this.memory_ }
      : { buffer: new ArrayBuffer(0) };
  }

  async load() {
    // @ts-ignore
    this.worker_ = new Worker(new URL("./worker/worker.ts", import.meta.url));
    this.loaded_ = true;
    this.status_ = "stopped";
    this.worker_.addEventListener("message", (evt) => this.onMessage(evt));
    this.worker_.addEventListener("error", (err) => this.onWorkerError(err));
    this.worker_.addEventListener("messageerror", (err) =>
      console.error("Worker Message Error:", err)
    );
  }

  async start(): Promise<boolean> {
    if (!this.loaded || !this.stopped) {
      throw new Error("Invalid Operation");
    }
    const response = await this.sendMessage({
      type: "start",
      id: this.nextId(),
    });
    if (messages.isErrorMessage(response)) {
      return false;
    } else if (messages.isStartedMessage(response)) {
      this.heap_ = response.heap;
    }
    return true;
  }

  addEventListener(type: "status", handler: StatusEventHandler): void;
  addEventListener(type: "write", handler: WriteCallback): void;
  addEventListener(type: "debug", handler: DebugBreakHander): void;

  addEventListener(type: unknown, handler: unknown): void {
    if (type === "status") {
      this.statusListeners.push(handler as StatusEventHandler);
    } else if (type === "write") {
      this.writeListeners.push(handler as WriteCallback);
    } else if (type === "debug") {
      this.debugListeners.push(handler as DebugBreakHander);
    }
  }

  removeEventListener(type: "status", handler: StatusEventHandler): void;
  removeEventListener(type: "write", handler: WriteCallback): void;
  removeEventListener(type: "debug", handler: DebugBreakHander): void;

  removeEventListener(type: unknown, handler: unknown): void {
    if (type === "status") {
      const idx = this.statusListeners.indexOf(handler as StatusEventHandler);
      if (idx >= 0) {
        this.statusListeners.splice(idx, 1);
      }
    } else if (type === "write") {
      const idx = this.writeListeners.indexOf(handler as WriteCallback);
      if (idx >= 0) {
        this.writeListeners.splice(idx, 1);
      }
    } else if (type === "debug") {
      const idx = this.debugListeners.indexOf(handler as DebugBreakHander);
      if (idx >= 0) {
        this.debugListeners.splice(idx, 1);
      }
    }
  }

  processLine(cmd: string) {
    this.postMessage({ type: "input", id: this.nextId(), content: cmd });
  }

  async print(ptr: number): Promise<string> {
    const response = await this.sendMessage({
      type: "print-req",
      id: this.nextId(),
      ptr: ptr,
    });
    if (messages.isPrintResponse(response)) {
      return response.content;
    }
    return "";
  }

  async getHeap(ptr: number): Promise<messages.HeapResponse> {
    const reqId = this.nextId();
    this.postMessage({ type: "heap-req", id: reqId, ptr: ptr });
    while (true) {
      const response = await this.getResponse(reqId);
      if (messages.isHeapResponse(response)) {
        return response;
      }
    }
  }

  async gcRun(collect: boolean): Promise<messages.GcResponse> {
    const response = await this.sendMessage({
      type: "gc-req",
      id: this.nextId(),
      collect: collect,
    });
    if (messages.isGcResponse(response)) {
      return response;
    }
    return {
      type: "gc-resp",
      id: response.id,
      output: "",
      isCollecting: false,
      collectionCount: 0,
      collected: 0,
      notCollected: 0,
      totalCollected: 0,
      totalNotCollected: 0,
    };
  }

  terminate() {
    if (!this.worker_) {
      return;
    }
    this.worker_.terminate();
    this.worker_ = undefined;
    this.loaded_ = false;
    this.status_ = "none";
    this.memory_ = undefined;
    this.heap_ = undefined;
    this.memorySize_ = 0;
    this.pendingResponses.clear();
    this.onOutput(`
\x1B[0;31mscheme runtime terminated\x1B[0m
`);
    this.statusListeners.forEach((el) => {
      el();
    });
  }

  async enableDebug(enabled: boolean): Promise<boolean> {
    if (!this.worker_) {
      return false;
    }
    const response = await this.sendMessage({
      type: "enable-debug",
      id: this.nextId(),
      enabled: enabled,
    });
    if (messages.isStatusResponse(response)) {
      return response.debugging;
    }
    return this.debugging;
  }

  async lookupEnv(env: number): Promise<messages.EnvResponse> {
    if (!this.worker_) {
      throw new Error("Invalid operation");
    }
    const response = await this.sendMessage({
      type: "env-req",
      id: this.nextId(),
      env: env,
    });
    if (messages.isEnvResponse(response)) {
      return response;
    }
    throw new Error("Invalid response");
  }

  private nextId() {
    return this.idCounter_++;
  }

  private onWorkerError(err: ErrorEvent) {
    this.onOutput(`\x1B[0;31m${err.message}\x1B[0m`);
  }

  private postMessage(msg: SendableMessages) {
    if (!this.worker_) {
      console.error("Cannot post message, no worker!");
    } else {
      this.worker_.postMessage(msg);
    }
  }

  private sendMessage(msg: SendableMessages): Promise<messages.WorkerMessage> {
    const promise = this.getResponse(msg.id);
    this.postMessage(msg);
    return promise;
  }

  private onMessage(evt: MessageEvent) {
    const cmd = evt.data;
    if (!messages.isWorkerMessage(cmd)) {
      console.warn("unrecognized message:", cmd);
      return;
    }

    if (messages.isStatusResponse(cmd)) {
      this.onStatus(cmd);
    } else if (messages.isOutputMessage(cmd)) {
      this.onOutput(cmd);
    } else if (messages.isMemoryMessage(cmd)) {
      this.onMemory(cmd);
    } else if (messages.isDebugBreak(cmd)) {
      this.onDebugBreak(cmd);
    } else if (messages.isErrorMessage(cmd)) {
      this.onErrorMessage(cmd);
    }

    if (this.pendingResponses.has(cmd.id)) {
      const resolver = this.pendingResponses.get(cmd.id);
      if (resolver) {
        this.pendingResponses.delete(cmd.id);
        resolver(cmd);
      }
    }
  }

  private onErrorMessage(cmd: messages.ErrorMessage) {
    this.onOutput(`\x1B[0;31m${cmd.err}\x1B[0m`);
  }

  private getResponse(id: number): Promise<messages.WorkerMessage> {
    return new Promise<messages.WorkerMessage>((resolve) => {
      this.pendingResponses.set(id, resolve);
    });
  }

  private onDebugBreak(cmd: messages.DebugBreak) {
    if (!this.debugListeners.length) {
      this.postMessage({ type: "debug-step", id: cmd.id, resultStep: false });
      return;
    }
    const event: DebugBreakEvent = {
      ...cmd,
      step: (resultStep: boolean) => {
        this.postMessage({
          type: "debug-step",
          id: cmd.id,
          resultStep: resultStep,
        });
      },
    };
    this.debugListeners.forEach((el) => el(event));
  }

  private onMemory(cmd: messages.MemoryMessage) {
    this.memory_ = cmd.memory;
  }

  private onStatus(cmd: messages.StatusResponse) {
    this.status_ = cmd.status;
    this.memorySize_ = cmd.memorySize;
    this.debugging_ = cmd.debugging;
    this.statusListeners.forEach((el) => {
      el();
    });
  }

  private onOutput(text: string): void;
  private onOutput(msg: messages.OutputMessage): void;

  private onOutput(arg: unknown): unknown {
    if (typeof arg === "string") {
      this.pendingOutput.push(arg);
    } else if (messages.isOutputMessage(arg)) {
      this.pendingOutput.push(arg.content);
    }

    if (this.outputCounter > 0) {
      return;
    } else {
      const frame = () => {
        if (this.outputCounter < 3) {
          this.outputCounter++;
          window.requestAnimationFrame(frame);
        } else {
          this.outputCounter = 0;

          const output = this.pendingOutput.join("");
          this.pendingOutput.splice(0, this.pendingOutput.length);
          this.writeListeners.forEach((el) => {
            el(output);
          });
        }
      };
      window.requestAnimationFrame(frame);
    }
  }
}
