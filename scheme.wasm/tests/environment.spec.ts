import { expect } from "chai";
import { create } from "domain";
import "mocha";

import { checkForLeaks, createString, IoTest, loadWasm } from "./common";

interface TestExports {
  memory: WebAssembly.Memory;
  gHeap: () => number;
  gTrue: () => number;
  gFalse: () => number;
  mallocInit: () => void;
  mallocFree: (ptr: number) => void;
  strFrom32: (len: number, val: number) => number;
  strFrom64: (len: number, val: bigint) => number;
  strFrom128: (len: number, val1: bigint, val2: bigint) => number;
  heapAlloc: (
    heap: number,
    type: number,
    data1: number,
    data2: number
  ) => number;
  runtimeInit: () => void;
  runtimeCleanup: () => void;
  environmentInit: (heap: number, outer: number) => number;
  environmentAdd: (env: number, key: number, value: number) => void;
  environmentGet: (env: number, key: number) => number;
  environmentSetBang: (env: number, key: number, value: number) => void;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    memory: instance.exports.memory as WebAssembly.Memory,
    gHeap: () => (instance.exports.gHeap as WebAssembly.Global).value as number,
    gTrue: () => (instance.exports.gTrue as WebAssembly.Global).value as number,
    gFalse: () =>
      (instance.exports.gFalse as WebAssembly.Global).value as number,
    mallocInit: instance.exports.mallocInit as () => void,
    mallocFree: instance.exports.mallocFree as (ptr: number) => void,
    strFrom32: instance.exports.strFrom32 as (
      len: number,
      val: number
    ) => number,
    strFrom64: instance.exports.strFrom64 as (
      len: number,
      val: bigint
    ) => number,
    strFrom128: instance.exports.strFrom128 as (
      len: number,
      val1: bigint,
      val2: bigint
    ) => number,
    heapAlloc: instance.exports.heapAlloc as (
      heap: number,
      type: number,
      data1: number,
      data2: number
    ) => number,
    runtimeInit: instance.exports.runtimeInit as () => void,
    runtimeCleanup: instance.exports.runtimeCleanup as () => void,
    environmentInit: instance.exports.environmentInit as (
      heap: number,
      outer: number
    ) => number,
    environmentAdd: instance.exports.environmentAdd as (
      env: number,
      key: number,
      value: number
    ) => void,
    environmentGet: instance.exports.environmentGet as (
      env: number,
      key: number
    ) => number,
    environmentSetBang: instance.exports.environmentSetBang as (
      env: number,
      key: number,
      value: number
    ) => void,
  };
}

describe("environment wasm", () => {
  const io = new IoTest();
  const wasm = loadWasm(io.module);
  let exports: TestExports;

  before(async () => {
    const instance = await wasm;
    exports = exportsFromInstance(instance);
    io.exports = exports;
    exports.mallocInit();
    exports.runtimeInit();
  });

  after(() => {
    exports.runtimeCleanup();
    checkForLeaks(exports);
  });

  const createHeapString = (str: string) => {
    const ptr = createString(exports, str);
    return exports.heapAlloc(exports.gHeap(), 7, ptr, 0);
  };

  it("can initialize an environment", () => {
    const env = exports.environmentInit(exports.gHeap(), 0);
    expect(env).to.be.greaterThan(exports.gHeap());
  });

  it("can add items to an environment", () => {
    const env = exports.environmentInit(exports.gHeap(), 0);

    const kTestItems: { key: string; value: string }[] = [
      { key: "item", value: "value" },
      { key: "bar", value: "bar stool" },
      { key: "baz", value: "haircut" },
    ];

    const stored: Record<string, number> = {};

    for (const { key, value } of kTestItems) {
      const heapKey = createHeapString(key);
      const heapValue = createHeapString(value);
      exports.environmentAdd(env, heapKey, heapValue);
      stored[key] = heapValue;
    }

    for (const { key, value } of kTestItems) {
      const heapKey = createHeapString(key);

      expect(exports.environmentGet(env, heapKey)).to.equal(stored[key]);
    }
  });

  it("can get items from environments", () => {
    const outer = exports.environmentInit(exports.gHeap(), 0);
    const inner = exports.environmentInit(exports.gHeap(), outer);

    const kOuterItems: { key: string; value: string }[] = [
      { key: "item", value: "value" },
      { key: "bar", value: "bar stool" },
      { key: "baz", value: "haircut" },
    ];

    const kInnerItems: { key: string; value: string }[] = [
      { key: "foo", value: "fighter" },
      { key: "bar", value: "bar stool" },
    ];

    const storedOuter: Record<string, number> = {};
    const storedInner: Record<string, number> = {};

    kOuterItems.forEach(({ key, value }) => {
      const heapKey = createHeapString(key);
      const heapValue = createHeapString(value);
      exports.environmentAdd(outer, heapKey, heapValue);
      storedOuter[key] = heapValue;
    });

    kInnerItems.forEach(({ key, value }) => {
      const heapKey = createHeapString(key);
      const heapValue = createHeapString(value);
      exports.environmentAdd(inner, heapKey, heapValue);
      storedInner[key] = heapValue;
    });

    expect(exports.environmentGet(inner, createHeapString("item"))).to.equal(
      storedOuter["item"],
      '"item" should be fetched from the outer environment'
    );
    expect(exports.environmentGet(inner, createHeapString("baz"))).to.equal(
      storedOuter["baz"],
      '"baz" should be fetched from the outer environment'
    );
    expect(exports.environmentGet(inner, createHeapString("bar"))).to.equal(
      storedInner["bar"],
      '"bar" should be fetched from the inner environment (shadowing)'
    );
    expect(exports.environmentGet(inner, createHeapString("foo"))).to.equal(
      storedInner["foo"],
      '"foo" should be fetched from the inner environment'
    );
    expect(exports.environmentGet(outer, createHeapString("bar"))).to.equal(
      storedOuter["bar"],
      '"bar" should be fetched from the outer environment (shadowed, fetched directly)'
    );
  });

  it("cannot double add an item", () => {
    const env = exports.environmentInit(exports.gHeap(), 0);

    exports.environmentAdd(
      env,
      createHeapString("foo"),
      createHeapString("bar")
    );
    expect(() =>
      exports.environmentAdd(
        env,
        createHeapString("foo"),
        createHeapString("baz")
      )
    ).throws("unreachable");
  });

  it("can overwrite an item with set!", () => {
    const env = exports.environmentInit(exports.gHeap(), 0);

    const heapFoo = createHeapString("foo");
    const heapBar = createHeapString("bar");
    const heapBaz = createHeapString("baz");

    exports.environmentAdd(env, heapFoo, heapBar);
    expect(exports.environmentGet(env, heapFoo)).to.equal(
      heapBar,
      'key "foo" should fetch "bar"'
    );

    exports.environmentSetBang(env, heapFoo, heapBaz);
    expect(exports.environmentGet(env, heapFoo)).to.equal(
      heapBaz,
      'after set!, key "foo" should fetch "baz"'
    );
  });
});
