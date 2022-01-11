import { SchemeRuntime } from "../SchemeRuntime";
import * as messages from "./messages";

class SchemeWorker {
  private runtime?: SchemeRuntime;
  private working_: boolean = false;

  constructor() {
    console.log("Started runtime worker");
    self.addEventListener("message", (ev) => this.onMessage(ev));
    this.onWrite = this.onWrite.bind(this);
  }

  postMessage(
    msg:
      | messages.StatusResponse
      | messages.OutputMessage
      | messages.PrintResponse
      | messages.StartedMessage
      | messages.GcResponse
  ) {
    self.postMessage(msg);
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
    } else {
      console.error("Unexpected message:", cmd);
    }
  }

  private onGcRequest(cmd: messages.GcRequest) {
    if (!this.runtime) {
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
    if (!this.runtime) {
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
    if (!this.runtime) {
      this.postMessage({ type: "print-resp", id: cmd.id, content: "" });
      return;
    }
    const output: string[] = [];
    const listener = (str: string) => {
      output.push(str);
    };
    this.runtime.addEventListener("write-priority", listener);
    this.runtime.print(cmd.ptr);
    this.runtime.removeEventListener("write-priority", listener);
    this.postMessage({
      type: "print-resp",
      id: cmd.id,
      content: output.join(""),
    });
  }

  private async onInput(cmd: messages.InputMessage) {
    if (!this.runtime) {
      return;
    }
    this.working_ = true;
    this.onStatus();
    await this.runtime.processLine(cmd.content);
    this.working_ = false;
    this.onStatus();
  }

  private async onStart(cmd: messages.StartMessage) {
    if (!this.runtime) {
      this.runtime = await SchemeRuntime.load();
      this.runtime.addEventListener("write", this.onWrite);
    }
    if (this.runtime.stopped) {
      await this.runtime.start();
    }
    this.postMessage({ type: "started", id: cmd.id, heap: this.runtime.gHeap });
    this.onStatus();
  }

  private onWrite(text: string) {
    this.postMessage({ type: "output", id: -1, content: text });
    this.onStatus();
  }

  private onStatus(cmd?: messages.WorkerMessage) {
    if (!this.runtime || this.runtime.stopped) {
      this.postMessage({
        type: "status-resp",
        id: cmd ? cmd.id : -1,
        status: "stopped",
        memorySize: 0,
      });
    } else {
      const memorySize = this.runtime.memory.buffer.byteLength;
      if (this.working_) {
        this.postMessage({
          type: "status-resp",
          id: cmd ? cmd.id : -1,
          status: "waiting",
          memorySize: memorySize,
        });
      } else if (this.runtime.partial) {
        this.postMessage({
          type: "status-resp",
          id: cmd ? cmd.id : -1,
          status: "partial",
          memorySize: memorySize,
        });
      } else {
        this.postMessage({
          type: "status-resp",
          id: cmd ? cmd.id : -1,
          status: this.runtime.waiting ? "waiting" : "running",
          memorySize: memorySize,
        });
      }
    }
  }
}

const worker = new SchemeWorker();
