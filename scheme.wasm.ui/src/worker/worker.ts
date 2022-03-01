import { SchemeRuntime } from "../SchemeRuntime";
import { SchemeType } from "../SchemeType";
import * as messages from "./messages";

type ResponseResolver = (msg: messages.WorkerMessage) => void;

type SendableMessages =
  | messages.DebugBreak
  | messages.EnvResponse
  | messages.ErrorMessage
  | messages.GcResponse
  | messages.OutputMessage
  | messages.PrintResponse
  | messages.StartedMessage
  | messages.StatusResponse;

class SchemeWorker {
  private runtime?: SchemeRuntime;
  private working_: boolean = false;
  private idCounter: number = 0x1000_0000;
  private readonly pendingResponses: Map<number, ResponseResolver> = new Map();
  private readonly output: string[] = [];

  constructor() {
    console.log("Started runtime worker");
    self.addEventListener("message", (ev) => this.onMessage(ev));
    this.onWrite = this.onWrite.bind(this);
    this.onDebug = this.onDebug.bind(this);
  }

  nextId() {
    return ++this.idCounter;
  }

  postMessage(msg: SendableMessages) {
    self.postMessage(msg);
  }

  sendMessage(msg: SendableMessages): Promise<messages.WorkerMessage> {
    const promise = this.getResponse(msg.id);
    this.postMessage(msg);
    return promise;
  }

  onMessage(ev: MessageEvent) {
    const cmd = ev.data;
    if (!messages.isWorkerMessage(cmd)) {
      console.warn("Unknown message:", cmd);
      return;
    }
    if (messages.isStatusRequest(cmd)) {
      this.onStatus(cmd);
    } else if (messages.isStartMessage(cmd)) {
      this.onStart(cmd);
    } else if (messages.isInputMessage(cmd)) {
      this.onInput(cmd);
    } else if (messages.isPrintRequest(cmd)) {
      this.onPrint(cmd);
    } else if (messages.isHeapRequest(cmd)) {
      this.onHeapRequest(cmd);
    } else if (messages.isGcRequest(cmd)) {
      this.onGcRequest(cmd);
    } else if (messages.isEnableDebug(cmd)) {
      this.onEnableDebug(cmd);
    } else if (messages.isEnvRequest(cmd)) {
      this.onEnvRequest(cmd);
    }

    const resolver = this.pendingResponses.get(cmd.id);
    if (resolver) {
      this.pendingResponses.delete(cmd.id);
      if (typeof resolver === "function") {
        resolver(cmd);
      }
    }
  }

  getResponse(id: number): Promise<messages.WorkerMessage> {
    return new Promise<messages.WorkerMessage>((resolve) => {
      this.pendingResponses.set(id, resolve);
    });
  }

  private onEnvRequest(cmd: messages.EnvRequest) {
    if (!this.runtime) {
      this.onStatus(cmd);
      return;
    }

    const heap = this.runtime.heapItem(cmd.env);
    const hashtable = heap[1];
    const parent = heap[2];

    const [capacity, count] = this.runtime.heapItem(hashtable, 2);
    const entries: messages.EnvEntry[] = [];
    let offset = 8;
    for (let i = 0; i < capacity; i++, offset += 16) {
      const hashtableEntry = this.runtime.heapItem(hashtable + offset, 4);
      const [digestLow, digestHigh, key, val] = hashtableEntry;

      if (
        digestLow === digestHigh &&
        (digestHigh == 0 || digestHigh == 0xffff_ffff)
      ) {
        // either empty or tombstone entry
        continue;
      }
      entries.push({
        name: this.runtime.getString(key),
        value: this.printPtr(val),
      });
    }

    if (entries.length !== count) {
      console.warn(
        `Environment ${cmd.env}, expecting ${count}, found ${entries.length}`
      );
    }

    this.postMessage({
      type: "env-resp",
      id: cmd.id,
      ptr: cmd.env,
      next: parent,
      entries: entries,
    });
  }

  private onEnableDebug(cmd: messages.EnableDebug) {
    if (!this.runtime || this.runtime.stopped) {
      return;
    }
    this.runtime.gDebug = cmd.enabled;
    this.onStatus(cmd);
  }

  private onGcRequest(cmd: messages.GcRequest) {
    if (!this.runtime || this.runtime.stopped) {
      this.onStatus(cmd);
      return;
    }
    const output: string[] = [];
    if (cmd.collect) {
      const listener = (str: string) => {
        output.push(str);
      };
      this.runtime.addEventListener("write-priority", listener);
      this.runtime.gcRun(this.runtime.replEnv);
      this.runtime.removeEventListener("write-priority", listener);
    }
    this.postMessage({
      type: "gc-resp",
      id: cmd.id,
      output: output.join(""),
      isCollecting: this.runtime.gGcIsCollecting,
      collectionCount: this.runtime.gGcCollectionCount,
      collected: this.runtime.gGcCollectedCount,
      notCollected: this.runtime.gGcNotCollectedCount,
      totalCollected: this.runtime.gGcTotalCollectedCount,
      totalNotCollected: this.runtime.gGcTotalNotCollectedCount,
    });
  }

  private onHeapRequest(cmd: messages.HeapRequest) {
    if (!this.runtime || this.runtime.stopped) {
      return;
    }
    const hdr = new Uint32Array(this.runtime.heapItem(cmd.ptr));
    const size = hdr[0] * 12;
    const buffer = new Uint8Array(size);
    buffer.set(
      new Uint8Array(
        this.runtime.memory.buffer.slice(cmd.ptr + 12, cmd.ptr + 12 + size)
      )
    );
    const resp: messages.HeapResponse = {
      type: "heap-resp",
      id: cmd.id,
      ptr: cmd.ptr,
      size: hdr[0],
      free: hdr[1],
      next: hdr[2],
      entries: buffer.buffer,
    };
    (self as unknown as Worker).postMessage(resp, [buffer.buffer]);
  }

  private onPrint(cmd: messages.PrintRequest) {
    if (!this.runtime || this.runtime.stopped) {
      this.postMessage({ type: "print-resp", id: cmd.id, content: "" });
      return;
    }
    this.postMessage({
      type: "print-resp",
      id: cmd.id,
      content: this.printPtr(cmd.ptr),
    });
  }

  private printPtr(ptr: number) {
    if (!this.runtime || this.runtime.stopped) {
      return "";
    }
    const output: string[] = [];
    const listener = (str: string) => {
      output.push(str);
    };
    this.runtime.addEventListener("write-priority", listener);
    this.runtime.print(ptr);
    this.runtime.removeEventListener("write-priority", listener);
    return output.join("");
  }

  private async onInput(cmd: messages.InputMessage) {
    if (!this.runtime || this.runtime.stopped) {
      return;
    }
    this.working_ = true;
    this.onStatus();
    await this.runtime.processLine(cmd.content);
    this.working_ = false;
    this.flushOutput();
    this.onStatus();
  }

  errorToString(err: unknown): string {
    if (err instanceof Error) {
      if (err.stack) {
        return err.stack;
      }
      return err.message;
    } else {
      return (err as any).toString();
    }
  }

  private async onStart(cmd: messages.StartMessage) {
    try {
      if (!this.runtime) {
        this.runtime = await SchemeRuntime.load();
        this.runtime.addEventListener("write", this.onWrite);
        this.runtime.addEventListener("debug", this.onDebug);
      }
      if (this.runtime.stopped) {
        await this.runtime.start();
      }
      this.postMessage({
        type: "started",
        id: cmd.id,
        heap: this.runtime.gHeap,
      });
    } catch (err) {
      this.postMessage({
        type: "error",
        id: cmd.id,
        err: this.errorToString(err),
      });
    }
    this.onStatus();
  }

  private onWrite(text: string) {
    this.output.push(text);
    // accumulate until there is a new line
    if (text.includes("\n")) {
      this.flushOutput();
    }
  }
  private flushOutput() {
    const text = this.output.join("");
    this.output.splice(0, this.output.length);
    this.postMessage({ type: "output", id: -1, content: text });
  }

  private onDebug(ptr: number, resolver: (resultStep: boolean) => void) {
    if (!this.runtime || this.runtime.stopped) {
      resolver(false);
      return;
    }

    const root = this.runtime.heapItem(ptr);
    if ((root[0] & SchemeType.Mask) !== SchemeType.Cont) {
      console.log("expecting continuation");
      resolver(false);
      return;
    }
    const rootCont = this.runtime.heapItem(root[1]);
    if (rootCont[1] === 0 && rootCont[2] === 0) {
      const first = this.runtime.heapItem(root[2]);
      if ((first[0] & SchemeType.Mask) !== SchemeType.Cont) {
        console.log("expecting continuation");
        resolver(false);
        return;
      }
      const firstCont = this.runtime.heapItem(first[1]);
      const firstArg = this.printPtr(firstCont[2]);
      const second = this.runtime.heapItem(first[2]);
      if ((second[0] & SchemeType.Mask) !== SchemeType.Cont) {
        console.log("expecting continuation");
        resolver(false);
        return;
      }
      const secondCont = this.runtime.heapItem(second[1]);
      const secondArg = this.printPtr(secondCont[2]);

      this.sendMessage({
        type: "debug-break",
        id: this.nextId(),
        ptr: ptr,
        env: firstCont[1],
        expr: `(${firstArg} ${secondArg.slice(1)}`,
      }).then((response) => {
        if (!messages.isDebugStep(response)) {
          resolver(false);
        } else {
          resolver(response.resultStep);
        }
      });
    } else {
      const args = this.runtime.heapItem(rootCont[2]);
      const expr =
        (args[0] & SchemeType.Mask) === SchemeType.Cons
          ? this.printPtr(args[1])
          : this.printPtr(rootCont[2]);

      // this is a result
      this.sendMessage({
        type: "debug-break",
        id: this.nextId(),
        ptr: ptr,
        env: rootCont[1],
        expr: expr,
      }).then((response) => resolver(false));
    }
  }

  private onStatus(cmd?: messages.WorkerMessage) {
    if (!this.runtime || this.runtime.stopped) {
      this.postMessage({
        type: "status-resp",
        id: cmd ? cmd.id : -1,
        status: "stopped",
        memorySize: 0,
        debugging: false,
      });
    } else {
      const memorySize = this.runtime.memory.buffer.byteLength;
      if (this.working_) {
        this.postMessage({
          type: "status-resp",
          id: cmd ? cmd.id : -1,
          status: "waiting",
          memorySize: memorySize,
          debugging: this.runtime.gDebug,
        });
      } else if (this.runtime.partial) {
        this.postMessage({
          type: "status-resp",
          id: cmd ? cmd.id : -1,
          status: "partial",
          memorySize: memorySize,
          debugging: this.runtime.gDebug,
        });
      } else {
        this.postMessage({
          type: "status-resp",
          id: cmd ? cmd.id : -1,
          status: this.runtime.waiting ? "waiting" : "running",
          memorySize: memorySize,
          debugging: this.runtime.gDebug,
        });
      }
    }
  }
}

const worker = new SchemeWorker();
