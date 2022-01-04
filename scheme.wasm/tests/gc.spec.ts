import { expect } from "chai";
import "mocha";

import {
  checkForLeaks,
  commonExportsFromInstance,
  CommonTestExports,
  createHeapSymbol,
  createString,
  getString,
  IoEvent,
  IoTest,
  loadWasm,
} from "./common";

interface TestExports extends CommonTestExports {
  memory: WebAssembly.Memory;
  mallocInit: () => void;
  runtimeInit: () => void;
  runtimeCleanup: () => void;
  environmentInit: (heap: number, outer: number) => number;
  read: () => number;
  eval: (env: number, expr: number) => number;
  registerBuiltins: (heap: number, env: number) => void;
  print: (ptr: number) => void;
  gGcIsCollecting: () => boolean;
  gGcCollectionCount: () => number;
  gGcTotalCollectedCount: () => number;
  gGcTotalNotCollectedCount: () => number;
  gGcCollectedCount: () => number;
  gGcNotCollectedCount: () => number;
  gcRun: (env: number) => void;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    ...commonExportsFromInstance(instance),
    memory: instance.exports.memory as WebAssembly.Memory,
    mallocInit: instance.exports.mallocInit as () => void,
    runtimeInit: instance.exports.runtimeInit as () => void,
    runtimeCleanup: instance.exports.runtimeCleanup as () => void,
    environmentInit: instance.exports.environmentInit as (
      heap: number,
      outer: number
    ) => number,
    read: instance.exports.read as () => number,
    eval: instance.exports.eval as (env: number, expr: number) => number,
    registerBuiltins: instance.exports.registerBuiltins as (
      heap: number,
      env: number
    ) => void,
    print: instance.exports.print as (ptr: number) => void,
    gGcIsCollecting: () =>
      !!(instance.exports.gGcIsCollecting as WebAssembly.Global).value,
    gGcCollectionCount: () =>
      (instance.exports.gGcCollectionCount as WebAssembly.Global)
        .value as number,
    gGcTotalCollectedCount: () =>
      (instance.exports.gGcTotalCollectedCount as WebAssembly.Global)
        .value as number,
    gGcTotalNotCollectedCount: () =>
      (instance.exports.gGcTotalNotCollectedCount as WebAssembly.Global)
        .value as number,
    gGcCollectedCount: () =>
      (instance.exports.gGcCollectedCount as WebAssembly.Global).value,
    gGcNotCollectedCount: () =>
      (instance.exports.gGcNotCollectedCount as WebAssembly.Global).value,
    gcRun: instance.exports.gcRun as (env: number) => void,
  };
}

describe("gc wasm", () => {
  const io = new IoTest();
  const wasm = loadWasm({ io: io.module });
  let exports: TestExports;
  const written: string[] = [];
  const writeHandler = (evt: IoEvent) => {
    if (evt.data) {
      written.push(evt.data);
    }
    return true;
  };

  before(async () => {
    const instance = await wasm;
    exports = exportsFromInstance(instance);
    io.exports = exports;
    exports.mallocInit();
    exports.runtimeInit();
    io.addEventListener("write", writeHandler);
  });

  after(() => {
    exports.runtimeCleanup();
    checkForLeaks(exports);
    io.removeEventListener("write", writeHandler);
  });

  afterEach(() => {
    if (written.length) {
      console.log(written.splice(0, written.length).join(""));
    }
  });

  it("Basic test of collection", () => {
    const tokens = ["(+ 1 (* 2 3))", "(+ 4 (* 5 6))"];
    const readHandler = (evt: IoEvent) => {
      evt.data = tokens.shift();
      return false;
    };
    io.addEventListener("read", readHandler);

    const env = exports.environmentInit(exports.gHeap(), 0);
    exports.registerBuiltins(exports.gHeap(), env);

    exports.print(exports.eval(env, exports.read()));
    expect(written.splice(0, written.length).join("")).to.equal("7");

    expect(exports.gGcCollectionCount()).to.equal(0);
    expect(exports.gGcIsCollecting()).to.be.false;
    expect(exports.gGcCollectedCount()).to.equal(0);
    expect(exports.gGcNotCollectedCount()).to.equal(0);
    exports.gcRun(env);
    expect(exports.gGcCollectionCount()).to.equal(1);
    expect(exports.gGcIsCollecting()).to.be.false;
    expect(exports.gGcCollectedCount()).to.be.greaterThan(0);
    expect(exports.gGcNotCollectedCount()).to.be.greaterThan(0);

    const collected = exports.gGcCollectedCount();
    const notCollected = exports.gGcNotCollectedCount();

    exports.print(exports.eval(env, exports.read()));
    expect(written.splice(0, written.length).join("")).to.equal("34");

    exports.gcRun(env);
    expect(exports.gGcCollectionCount()).to.equal(2);
    expect(exports.gGcIsCollecting()).to.be.false;
    expect(collected + exports.gGcCollectedCount()).to.equal(
      exports.gGcTotalCollectedCount()
    );
    expect(notCollected + exports.gGcNotCollectedCount()).to.equal(
      exports.gGcTotalNotCollectedCount()
    );

    io.removeEventListener("read", readHandler);
  });
});
