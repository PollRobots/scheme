import fs from 'fs/promises';

export async function loadWasm(): Promise<WebAssembly.Instance> {
  const wasm = await fs.readFile("dist/test.wasm");
  try {
    const module = await WebAssembly.instantiate(wasm, {});
    return module.instance;
  } catch (err) {
    console.error(err);
    throw err;
  }
}
