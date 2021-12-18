#!/usr/bin/env node
import fs from "fs/promises";
import { Command, OptionValues } from "commander";
import wabt from "wabt";
import { emit, parse } from "./watmacro";

type WasmNumericType = "i32" | "i64";

function isWasmNumericType(value: any): value is WasmNumericType {
  return value === "i32" || value === "i64";
}

interface ConfigImportFunc {
  module: string;
  name: string;
  index: string;
  params: WasmNumericType[];
  result?: WasmNumericType;
}

function isConfigImportFunc(value: any): value is ConfigImportFunc {
  return (
    value &&
    Reflect.has(value, "module") &&
    typeof value.module === "string" &&
    Reflect.has(value, "name") &&
    typeof value.name === "string" &&
    Reflect.has(value, "index") &&
    typeof value.index === "string" &&
    Reflect.has(value, "params") &&
    Array.isArray(value.params) &&
    (value.params as any[]).every(isWasmNumericType) &&
    (value.result === undefined || isWasmNumericType(value.result))
  );
}

function isConfigImportFuncArray(value: any): value is ConfigImportFunc[] {
  return value && Array.isArray(value) && value.every(isConfigImportFunc);
}

interface ConfigExport {
  name: string;
  type: "func" | "table" | "memory" | "global";
  index: string;
}

function isConfigExport(value: any): value is ConfigExport {
  return (
    value &&
    Reflect.has(value, "name") &&
    typeof value.name === "string" &&
    Reflect.has(value, "type") &&
    (value.type === "func" ||
      value.type === "table" ||
      value.type === "memory" ||
      value.type === "global") &&
    Reflect.has(value, "index") &&
    typeof value.index === "string"
  );
}

function isConfigExportArray(value: any): value is ConfigExport[] {
  return value && Array.isArray(value) && value.every(isConfigExport);
}

interface ConfigMemory {
  name: string;
  limit: number;
}

function isConfigMemory(value: any): value is ConfigMemory {
  return (
    value &&
    Reflect.has(value, "name") &&
    typeof value.name === "string" &&
    Reflect.has(value, "limit") &&
    typeof value.limit === "number"
  );
}

interface WabtOptions {
  wasmFeatures: any;
  binaryOptions: any;
}

interface BuildWatConfig {
  memory?: ConfigMemory;
  files: string[];
  start?: string;
  importFuncs?: ConfigImportFunc[];
  exports?: ConfigExport[];
  wabtOptions?: WabtOptions;
}

function isStringArray(value: any): value is string[] {
  return (
    value && Array.isArray(value) && value.every((el) => typeof el === "string")
  );
}

function isBuildWatConfig(value: any): value is BuildWatConfig {
  return (
    value &&
    (!Reflect.has(value, "memory") || isConfigMemory(value.memory)) &&
    Reflect.has(value, "files") &&
    isStringArray(value.files) &&
    (!Reflect.has(value, "start") || typeof value.start === "string") &&
    (!Reflect.has(value, "importFuncs") ||
      isConfigImportFuncArray(value.importFuncs)) &&
    (!Reflect.has(value, "exports") || isConfigExportArray(value.exports))
  );
}

function makePrefix(config: BuildWatConfig): string[] {
  const prefix = ["(module"];
  if (config.importFuncs) {
    config.importFuncs.forEach((el) => {
      const module = JSON.stringify(el.module);
      const name = JSON.stringify(el.name);
      const params = el.params.map((t) => ` (param ${t})`).join("");
      const result = el.result ? ` (result ${el.result})` : "";
      prefix.push(
        `  (func ${el.index} (import ${module} ${name})${params}${result})`
      );
    });
  }
  if (config.memory) {
    if (!config.memory.name.startsWith("$")) {
      console.warn("Invalid memory name: ", config.memory.name);
    }
    if (config.memory.limit != (config.memory.limit | 0)) {
      console.warn("Invalid memory limit: ", config.memory.limit);
    }
    prefix.push(`  (memory ${config.memory.name} ${config.memory.limit | 0})`);
  }
  return prefix;
}

function makeSuffix(config: BuildWatConfig): string[] {
  const suffix = [""];

  if (config.start) {
    if (!config.start.startsWith("$")) {
      console.warn("Invalid start name: ", config.start);
    }
    suffix.push(`  (start ${config.start})`);
  }

  if (config.exports) {
    config.exports.forEach((el) => {
      if (!el.index.startsWith("$")) {
        console.warn("Invalid export index: ", el.index);
      }
      suffix.push(
        `  (export ${JSON.stringify(el.name)} (${el.type} ${el.index}))`
      );
    });
  }

  suffix.push(")");
  return suffix;
}

async function getLastInputEdit(files: string[]) {
  let latest = new Date(0);
  for (const file of files) {
    const stat = await fs.stat(file);
    const mtime = new Date(stat.mtimeMs);
    if (mtime > latest) {
      latest = mtime;
    }
  }
  return latest;
}

function stripNonAscii(input: string): string {
  return Array.from(input)
    .map((el) => ((el.codePointAt(0) || 0) > 127 ? "_" : el))
    .join("");
}

async function main() {
  const program = new Command();
  program
    .name("buildwat")
    .description("Concatenates and assembles a set of .WAT files.")
    .version("0.0.0.1")
    .requiredOption("-o, --output <wasm file>", "The output to generate")
    .option("-t, --tempfile <tempfile>", "Save the intermediate WAT file")
    .option("-m, --macros", "Use macro expansion")
    .option("-f, --force", "Build even if output is older than inputs")
    .argument("<config>", "build configuration file");

  program.parse();
  const opts =
    program.opts<{
      output: string;
      tempfile?: string;
      macros?: boolean;
      force?: boolean;
    }>();

  const config = JSON.parse(await fs.readFile(program.args[0], "utf-8"));
  if (!isBuildWatConfig(config)) {
    throw new Error(`Config file '${program.args[0]}' is invalid.`);
  }

  if (!opts.force) {
    try {
      const lastInputEdit = await getLastInputEdit([
        ...config.files,
        program.args[0],
      ]);
      const lastOutputEdit = await getLastInputEdit([opts.output]);
      if (lastOutputEdit > lastInputEdit) {
        console.log(`${program.name()}: build skipped.`);
        return;
      }
    } catch (err) {}
  }

  const wabtModule = await wabt();
  const contents: string[] = [];
  contents.push(...makePrefix(config));

  for (const el of config.files) {
    const content = await fs.readFile(el, "utf-8");
    contents.push(
      "",
      "  ;;;;;;;;;;;;;;;;;;;;",
      `  ;; ${el}`,
      "  ;;;;;;;;;;;;;;;;;;;;",
      "",
      stripNonAscii(content)
    );
  }

  contents.push(...makeSuffix(config));
  let joined = contents.join("\n");

  if (opts.macros) {
    try {
      const parsed = parse(joined);
      const expanded = emit(parsed);
      console.log("Expanded macros");
      joined = expanded.join("");
    } catch (e) {
      console.error("Error expanding macros");
      console.error(
        Reflect.has(e as object, "message")
          ? (e as { message: string }).message
          : e
      );
    }
  }

  if (opts.tempfile) {
    await fs.writeFile(opts.tempfile, joined, "utf-8");
    console.log(`Intermediate file written as '${opts.tempfile}'`);
  }
  const module = wabtModule.parseWat(
    opts.tempfile || "buildwat.tmp",
    joined,
    config.wabtOptions ? config.wabtOptions.wasmFeatures || {} : {}
  );
  const { buffer: wasm, log } = module.toBinary(
    config.wabtOptions ? config.wabtOptions.binaryOptions || {} : {}
  );
  if (log) {
    console.log(log);
  }
  await fs.writeFile(opts.output, wasm);
  console.log(
    `Assembled ${config.files.length} files into a wasm module of ${wasm.byteLength} bytes, wrote to '${opts.output}'`
  );
}

(async () => {
  try {
    await main();
  } catch (err) {
    if (Reflect.has(err as object, "message")) {
      console.error((err as { message: string }).message);
    } else {
      console.error(err);
    }
    process.exitCode = 1;
  }
})();
