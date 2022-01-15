import { FlushValues } from "pako";

export interface WorkerMessage {
  type: string;
  id: number;
}

export function isWorkerMessage(value: any): value is WorkerMessage {
  return (
    value && typeof value.type === "string" && typeof value.id === "number"
  );
}

export interface StatusRequest extends WorkerMessage {
  type: "status-req";
}

export function isStatusRequest(value: any): value is StatusRequest {
  return isWorkerMessage(value) && value.type === "status-req";
}

export interface StatusResponse extends WorkerMessage {
  type: "status-resp";
  status: "stopped" | "waiting" | "running" | "partial";
  memorySize: number;
  debugging: boolean;
}

export function isStatusResponse(value: any): value is StatusResponse {
  return (
    value &&
    value.type === "status-resp" &&
    typeof value.id === "number" &&
    typeof value.status === "string" &&
    (value.status === "stopped" ||
      value.status === "waiting" ||
      value.status === "running" ||
      value.status === "partial") &&
    typeof value.memorySize === "number" &&
    typeof value.debugging === "boolean"
  );
}

export interface ErrorMessage extends WorkerMessage {
  type: "error";
  err: string;
}

export function isErrorMessage(value: any): value is ErrorMessage {
  return value && value.type === "error" && typeof value.err === "string";
}

export interface OutputMessage extends WorkerMessage {
  type: "output";
  content: string;
}

export function isOutputMessage(value: any): value is OutputMessage {
  return (
    value &&
    value.type === "output" &&
    typeof value.id === "number" &&
    typeof value.content === "string"
  );
}

export interface InputMessage extends WorkerMessage {
  type: "input";
  content: string;
}

export function isInputMessage(value: any): value is InputMessage {
  return (
    value &&
    value.type === "input" &&
    typeof value.id === "number" &&
    typeof value.content === "string"
  );
}

export interface StartMessage extends WorkerMessage {
  type: "start";
}

export function isStartMessage(value: any): value is StartMessage {
  return isWorkerMessage(value) && value.type === "start";
}

export interface MemoryMessage extends WorkerMessage {
  type: "memory";
  memory: ArrayBuffer;
}

export interface StartedMessage extends WorkerMessage {
  type: "started";
  heap: number;
}

export function isStartedMessage(value: any): value is StartedMessage {
  return value && value.type === "started" && typeof value.heap === "number";
}

export function isMemoryMessage(value: any): value is MemoryMessage {
  return (
    value &&
    value.type === "memory" &&
    typeof value.memory === "object" &&
    value.memory instanceof ArrayBuffer
  );
}

export interface PrintRequest extends WorkerMessage {
  type: "print-req";
  ptr: number;
}

export function isPrintRequest(value: any): value is PrintRequest {
  return (
    value &&
    value.type === "print-req" &&
    typeof value.id === "number" &&
    typeof value.ptr === "number"
  );
}

export interface PrintResponse extends WorkerMessage {
  type: "print-resp";
  content: string;
}

export function isPrintResponse(value: any): value is PrintResponse {
  return (
    value &&
    value.type === "print-resp" &&
    typeof value.id === "number" &&
    typeof value.content === "string"
  );
}

export interface HeapRequest extends WorkerMessage {
  type: "heap-req";
  ptr: number;
}

export function isHeapRequest(value: any): value is HeapRequest {
  return value && value.type === "heap-req" && typeof value.ptr === "number";
}

export interface HeapResponse extends WorkerMessage {
  type: "heap-resp";
  ptr: number;
  size: number;
  free: number;
  next: number;
  entries: ArrayBuffer;
}

export function isHeapResponse(value: any): value is HeapResponse {
  return (
    value &&
    value.type === "heap-resp" &&
    typeof value.ptr === "number" &&
    typeof value.size === "number" &&
    typeof value.free === "number" &&
    typeof value.next === "number" &&
    typeof value.entries === "object" &&
    value.entries instanceof ArrayBuffer
  );
}

export interface GcRequest extends WorkerMessage {
  type: "gc-req";
  collect: boolean;
}

export function isGcRequest(value: any): value is GcRequest {
  return (
    value &&
    typeof value.id === "number" &&
    value.type === "gc-req" &&
    typeof value.collect === "boolean"
  );
}

export interface GcResponse extends WorkerMessage {
  type: "gc-resp";
  output: string;
  isCollecting: boolean;
  collectionCount: number;
  collected: number;
  notCollected: number;
  totalCollected: number;
  totalNotCollected: number;
}

export function isGcResponse(value: any): value is GcResponse {
  return (
    value &&
    value.type == "gc-resp" &&
    typeof value.id === "number" &&
    typeof value.output === "string" &&
    typeof value.collectionCount === "number" &&
    typeof value.collected === "number" &&
    typeof value.notCollected === "number" &&
    typeof value.totalCollected === "number" &&
    typeof value.totalNotCollected === "number"
  );
}

export interface EnableDebug extends WorkerMessage {
  type: "enable-debug";
  enabled: boolean;
}

export function isEnableDebug(value: any): value is EnableDebug {
  return (
    value &&
    value.type === "enable-debug" &&
    typeof value.id === "number" &&
    typeof value.enabled === "boolean"
  );
}

export interface DebugBreak extends WorkerMessage {
  type: "debug-break";
  ptr: number;
  env: number;
  expr: string;
}

export function isDebugBreak(value: any): value is DebugBreak {
  return (
    value &&
    value.type === "debug-break" &&
    typeof value.id === "number" &&
    typeof value.ptr === "number" &&
    typeof value.env === "number" &&
    typeof value.expr === "string"
  );
}

export interface DebugStep extends WorkerMessage {
  type: "debug-step";
  resultStep: boolean;
}

export function isDebugStep(value: any): value is DebugStep {
  return (
    value &&
    value.type === "debug-step" &&
    typeof value.id === "number" &&
    typeof value.resultStep === "boolean"
  );
}

export interface EnvRequest extends WorkerMessage {
  type: "env-req";
  env: number;
}

export function isEnvRequest(value: any): value is EnvRequest {
  return (
    value &&
    value.type === "env-req" &&
    typeof value.id === "number" &&
    typeof value.env === "number"
  );
}

export interface EnvEntry {
  name: string;
  value: string;
}

export function isEnvEntry(value: any): value is EnvEntry {
  return (
    value && typeof value.name === "string" && typeof value.value === "string"
  );
}

export interface EnvResponse extends WorkerMessage {
  type: "env-resp";
  ptr: number;
  next: number;
  entries: EnvEntry[];
}

export function isEnvResponse(value: any): value is EnvResponse {
  return (
    value &&
    value.type === "env-resp" &&
    typeof value.id === "number" &&
    typeof value.ptr === "number" &&
    typeof value.next === "number" &&
    Array.isArray(value.entries) &&
    (value.entries as any[]).every(isEnvEntry)
  );
}
