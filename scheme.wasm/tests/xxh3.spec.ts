import { expect } from "chai";
import "mocha";
import { loadWasm } from "./common";

interface TestExports {
  memory: WebAssembly.Memory;
  mallocInit: () => void;
  malloc: (size: number) => number;
  mallocFree: (ptr: number) => void;
  xxh64: (ptr: number, len: number, seed: bigint) => bigint;
}

function exportsFromInstance(instance: WebAssembly.Instance): TestExports {
  return {
    memory: instance.exports.memory as WebAssembly.Memory,
    mallocInit: instance.exports.mallocInit as () => void,
    malloc: instance.exports.malloc as (size: number) => number,
    mallocFree: instance.exports.mallocFree as (ptr: number) => void,
    xxh64: instance.exports.xxh64 as (
      ptr: number,
      len: number,
      seed: bigint
    ) => bigint,
  };
}

const kLongString = new Array(200)
  .fill(0)
  .map((el, i) => (i % 10).toString())
  .join("");

/*
Test vectors were generated using python

>>> import xxhash

>>> xxhash.xxh64(b'aaaa').digest().hex()
'42a70d1abf84bf32'
>>> xxhash.xxh64(b'Hello, World!').digest().hex()
'c49aacf8080fe47f'
>>> xxhash.xxh64(b'The quick brown fox jumps over the lazy dog').digest().hex()
'0b242d361fda71bc'
>>> s = b'0123456789' * 20
>>> xxhash.xxh64(s).digest().hex()
'2561e865c1554c7e'
>>> print('\n'.join(["[kLongString.substring(0, {}), '{}'],".format(i, xxhash.xxh64(s[:i]).digest().hex()) for i in range(32,65)]))
[kLongString.substring(0, 32), 'e5cc9f411ea110ba'],
[kLongString.substring(0, 33), '14c504c80731f0bd'],
[kLongString.substring(0, 34), 'c45b3a3e62f069b5'],
.
.
.
*/

const test_vectors: [string, string][] = [
  // v-short
  ["aaaa", "42a70d1abf84bf32"],
  // short
  ["Hello, World!", "c49aacf8080fe47f"],
  // medium
  ["The quick brown fox jumps over the lazy dog", "0b242d361fda71bc"],

  // long
  [kLongString, "2561e865c1554c7e"],

  // range of string lengths covering a 32 byte length range, this should exercise all basis paths
  [kLongString.substring(0, 32), "e5cc9f411ea110ba"],
  [kLongString.substring(0, 33), "14c504c80731f0bd"],
  [kLongString.substring(0, 34), "c45b3a3e62f069b5"],
  [kLongString.substring(0, 35), "0c1951de35e7ead1"],
  [kLongString.substring(0, 36), "a9a7b43e248fa3e1"],
  [kLongString.substring(0, 37), "3f3df7c8777eb8f1"],
  [kLongString.substring(0, 38), "ca696f7ffdb7fbed"],
  [kLongString.substring(0, 39), "4a87bfe8a7538329"],
  [kLongString.substring(0, 40), "ca6fc80cbde1a931"],
  [kLongString.substring(0, 41), "64e0f213a4dd18a2"],
  [kLongString.substring(0, 42), "8869b856c3b0cd2e"],
  [kLongString.substring(0, 43), "b8275be97ac4b50a"],
  [kLongString.substring(0, 44), "8214670ec0109053"],
  [kLongString.substring(0, 45), "937a56187cdd5d2e"],
  [kLongString.substring(0, 46), "5acf9f85a9adaf4d"],
  [kLongString.substring(0, 47), "6d0c48a1bc492eba"],
  [kLongString.substring(0, 48), "91b6e0fee81d1895"],
  [kLongString.substring(0, 49), "de09dfbaa69b2258"],
  [kLongString.substring(0, 50), "4f7ca65914623935"],
  [kLongString.substring(0, 51), "e8fd8f5935642a95"],
  [kLongString.substring(0, 52), "4a2ec8a8334ea2bc"],
  [kLongString.substring(0, 53), "b124bf8cf62882c7"],
  [kLongString.substring(0, 54), "202a8b8ac9ec77a6"],
  [kLongString.substring(0, 55), "7884d06d101bac76"],
  [kLongString.substring(0, 56), "01e352ac5c4a8448"],
  [kLongString.substring(0, 57), "97a0c685d5d8d8ec"],
  [kLongString.substring(0, 58), "e07749bf6bb686a5"],
  [kLongString.substring(0, 59), "34d8b3c4d29853a2"],
  [kLongString.substring(0, 60), "66ea70f4211f2a4f"],
  [kLongString.substring(0, 61), "d8854214af794c4a"],
  [kLongString.substring(0, 62), "f861d51a7263f787"],
  [kLongString.substring(0, 63), "93a9b4352b475d35"],
  [kLongString.substring(0, 64), "d502f0d566ce31d4"],
];

describe("xxh3 wasm", () => {
  const wasm = loadWasm();

  it("Test vectors hash to consistent values", async () => {
    const instance = await wasm;
    const exports = exportsFromInstance(instance);

    exports.mallocInit();

    for (const [input, output] of test_vectors) {
      const buffer = Buffer.from(input, "utf-8");
      const arr = new Uint8Array(buffer);

      const ptr = exports.malloc(buffer.byteLength);
      const view = new Uint8Array(exports.memory.buffer);

      for (let i = 0; i != arr.length; i++) {
        view[ptr + i] = arr[i];
      }
      const hash = exports.xxh64(ptr, arr.length, 0n);
      const hexHash = BigInt.asUintN(64, hash).toString(16).padStart(16, "0");

      expect(hexHash).to.equal(
        output,
        `Taking xxh64 digest of ${input.length} byte string ${input}`
      );

      exports.mallocFree(ptr);
    }
  });
});
