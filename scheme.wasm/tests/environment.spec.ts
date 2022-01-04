import { expect } from "chai";
import "mocha";

import {
  checkForLeaks,
  commonExportsFromInstance,
  CommonTestExports,
  createHeapString,
  createHeapSymbol,
  IoTest,
  loadWasm,
} from "./common";

interface TestExports extends CommonTestExports {
  gTrue: () => number;
  gFalse: () => number;
  mallocInit: () => void;
  mallocFree: (ptr: number) => void;
  runtimeInit: () => void;
  runtimeCleanup: () => void;
  environmentInit: (heap: number, outer: number) => number;
  environmentAdd: (env: number, key: number, value: number) => void;
  environmentGet: (env: number, key: number) => number;
  environmentSetBang: (env: number, key: number, value: number) => void;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    ...commonExportsFromInstance(instance),
    gTrue: () => (instance.exports.gTrue as WebAssembly.Global).value as number,
    gFalse: () =>
      (instance.exports.gFalse as WebAssembly.Global).value as number,
    mallocInit: instance.exports.mallocInit as () => void,
    mallocFree: instance.exports.mallocFree as (ptr: number) => void,
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
  const wasm = loadWasm({ io: io.module });
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
      const heapKey = createHeapSymbol(exports, key);
      const heapValue = createHeapString(exports, value);
      exports.environmentAdd(env, heapKey, heapValue);
      stored[key] = heapValue;
    }

    for (const { key, value } of kTestItems) {
      const heapKey = createHeapSymbol(exports, key);

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
      const heapKey = createHeapSymbol(exports, key);
      const heapValue = createHeapString(exports, value);
      exports.environmentAdd(outer, heapKey, heapValue);
      storedOuter[key] = heapValue;
    });

    kInnerItems.forEach(({ key, value }) => {
      const heapKey = createHeapSymbol(exports, key);
      const heapValue = createHeapString(exports, value);
      exports.environmentAdd(inner, heapKey, heapValue);
      storedInner[key] = heapValue;
    });

    expect(
      exports.environmentGet(inner, createHeapSymbol(exports, "item"))
    ).to.equal(
      storedOuter["item"],
      '"item" should be fetched from the outer environment'
    );
    expect(
      exports.environmentGet(inner, createHeapSymbol(exports, "baz"))
    ).to.equal(
      storedOuter["baz"],
      '"baz" should be fetched from the outer environment'
    );
    expect(
      exports.environmentGet(inner, createHeapSymbol(exports, "bar"))
    ).to.equal(
      storedInner["bar"],
      '"bar" should be fetched from the inner environment (shadowing)'
    );
    expect(
      exports.environmentGet(inner, createHeapSymbol(exports, "foo"))
    ).to.equal(
      storedInner["foo"],
      '"foo" should be fetched from the inner environment'
    );
    expect(
      exports.environmentGet(outer, createHeapSymbol(exports, "bar"))
    ).to.equal(
      storedOuter["bar"],
      '"bar" should be fetched from the outer environment (shadowed, fetched directly)'
    );
  });

  it("cannot double add an item", () => {
    const env = exports.environmentInit(exports.gHeap(), 0);

    exports.environmentAdd(
      env,
      createHeapSymbol(exports, "foo"),
      createHeapString(exports, "bar")
    );
    expect(() =>
      exports.environmentAdd(
        env,
        createHeapSymbol(exports, "foo"),
        createHeapString(exports, "baz")
      )
    ).throws("unreachable");
  });

  it("can overwrite an item with set!", () => {
    const env = exports.environmentInit(exports.gHeap(), 0);

    const heapFoo = createHeapSymbol(exports, "foo");
    const heapBar = createHeapString(exports, "bar");
    const heapBaz = createHeapString(exports, "baz");

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
