#!/usr/bin/env node

import fs from "fs/promises";
import https from "https";
import readline from "readline";
import stream from "stream";
import path from "path";
import zlib from "zlib";

const kUnicodeDataUrl =
  "https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt";

const kDerivedCorePropertiesUrl =
  "https://www.unicode.org/Public/UCD/latest/ucd/DerivedCoreProperties.txt";

const kBlocksJsonGz = "dist/unicode/blocks.json.gz";

async function loadTextFile(url: string): Promise<stream.Readable> {
  return new Promise<stream.Readable>((resolve, reject) => {
    const request = https.get(url, {}, (res) => {
      if (res.statusCode === undefined) {
        return reject(new Error("Result has no status code?"));
      }
      if (res.statusCode < 200 || res.statusCode >= 300) {
        return reject(new Error(`Status code: ${res.statusCode}`));
      }

      res.setEncoding("utf-8");

      resolve(res);
    });

    request.on("error", reject);
    request.end();
  });
}

function loadUnicodeText(): Promise<stream.Readable> {
  return loadTextFile(kUnicodeDataUrl);
}

class NamedRange {
  private readonly name_: string;
  private readonly ranges: [number, number][] = [];
  private count_: number = 0;

  constructor(name: string) {
    this.name_ = name;
  }

  addRange(from: number, to: number) {
    this.ranges.push([from, to]);
    this.count_ += 1 + to - from;
  }

  get name(): string {
    return this.name_;
  }
  get count(): number {
    return this.count_;
  }

  isInRange(codePoint: number): boolean {
    for (const [from, to] of this.ranges) {
      if (from > codePoint) {
        return false;
      }
      if (from <= codePoint && codePoint <= to) {
        return true;
      }
    }
    return false;
  }
}

/*
0009..000D    ; White_Space # Cc   [5] <control-0009>..<control-000D>
0020          ; White_Space # Zs       SPACE
0085          ; White_Space # Cc       <control-0085>
00A0          ; White_Space # Zs       NO-BREAK SPACE
1680          ; White_Space # Zs       OGHAM SPACE MARK
2000..200A    ; White_Space # Zs  [11] EN QUAD..HAIR SPACE
2028          ; White_Space # Zl       LINE SEPARATOR
2029          ; White_Space # Zp       PARAGRAPH SEPARATOR
202F          ; White_Space # Zs       NARROW NO-BREAK SPACE
205F          ; White_Space # Zs       MEDIUM MATHEMATICAL SPACE
3000          ; White_Space # Zs       IDEOGRAPHIC SPACE
*/
function whitespaceRange(): NamedRange {
  const range = new NamedRange("White_Space");
  range.addRange(0x0009, 0x000d); // <control-0009>..<control-000D>
  range.addRange(0x0020, 0x0020); // SPACE
  range.addRange(0x0085, 0x0085); // <control-0085>
  range.addRange(0x00a0, 0x00a0); // NO-BREAK SPACE
  range.addRange(0x1680, 0x1680); // OGHAM SPACE MARK
  range.addRange(0x2000, 0x200a); // EN QUAD..HAIR SPACE
  range.addRange(0x2028, 0x2028); // LINE SEPARATOR
  range.addRange(0x2029, 0x2029); // PARAGRAPH SEPARATOR
  range.addRange(0x202f, 0x202f); // NARROW NO-BREAK SPACE
  range.addRange(0x205f, 0x205f); // MEDIUM MATHEMATICAL SPACE
  range.addRange(0x3000, 0x3000); // IDEOGRAPHIC SPACE

  return range;
}

async function loadDerivedCoreProperties(): Promise<NamedRange[]> {
  const coreprops = await loadTextFile(kDerivedCorePropertiesUrl);
  const rl = readline.createInterface({
    input: coreprops,
    crlfDelay: Infinity,
  });

  const alphabetic = new NamedRange("Alphabetic");
  const uppercase = new NamedRange("Uppercase");
  const lowercase = new NamedRange("Lowercase");

  for await (const line of rl) {
    if (line.startsWith("#") || line.trim().length === 0) {
      // skip blank lines and comments
      continue;
    }

    const re = /^([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s+;\s+([\w_]+)\s+#/;
    const match = line.match(re);
    if (!match) {
      console.log(line);
      continue;
    }

    const from = parseInt(match[1], 16);
    const to = match[2] !== undefined ? parseInt(match[2], 16) : from;

    if (match[3] === "Alphabetic") {
      alphabetic.addRange(from, to);
    } else if (match[3] === "Uppercase") {
      uppercase.addRange(from, to);
    } else if (match[3] === "Lowercase") {
      lowercase.addRange(from, to);
    }
  }
  return [alphabetic, uppercase, lowercase];
}

function writeBlock(index: number, block: Uint8Array): Promise<void> {
  const name = `dist/unicode/${index.toString(16).padStart(4, "0")}.bin`;
  return fs.writeFile(name, block);
}

function gzip(input: string): Promise<Buffer> {
  const buffer = Buffer.from(input);
  const gz = zlib.createGzip();
  return new Promise<Buffer>((resolve, reject) =>
    zlib.gzip(buffer, (err, buf) => {
      if (err) {
        reject(err);
      } else {
        resolve(buf);
      }
    })
  );
}

async function writeBlockList(blocks: Record<number, string>) {
  const content = JSON.stringify(blocks);
  const buffer = await gzip(content);
  return fs.writeFile(kBlocksJsonGz, buffer);
}

function setCodePoint(block: Uint8Array, offset: number, codePoint: number) {
  block[offset] = codePoint & 0xff;
  block[offset + 1] = (codePoint >> 8) & 0xff;
  block[offset + 2] = (codePoint >> 16) & 0x1f;
}

async function main(): Promise<void> {
  try {
    await fs.access(kBlocksJsonGz);
    console.log("Skipping Unicode data already exists");
    return;
  } catch (err) {}

  const start = performance.now();
  const [alphabetic, uppercase, lowercase] = await loadDerivedCoreProperties();
  const whitespace = whitespaceRange();
  console.log(`${alphabetic.name}: ${alphabetic.count}`);
  console.log(`${uppercase.name}: ${uppercase.count}`);
  console.log(`${lowercase.name}: ${lowercase.count}`);
  console.log(`${whitespace.name}: ${whitespace.count}`);

  const stream = await loadUnicodeText();
  const rl = readline.createInterface({
    input: stream,
    terminal: false,
    crlfDelay: Infinity,
  });

  await fs.mkdir("dist/unicode", { recursive: true });

  const block = new Uint8Array(256 * 8);
  let blockIndex = 0;
  let count = 0;
  const blocks: Record<number, string> = {};

  for await (const line of rl) {
    count++;
    const parts = line.split(";");
    const codePoint = parseInt(parts[0], 16);
    const targetBlock = codePoint >> 8;
    if (targetBlock != blockIndex) {
      await writeBlock(blockIndex, block);
      blocks[blockIndex] = Buffer.from(block).toString("base64");
      blockIndex = targetBlock;
    }
    const row = codePoint & 0xff;
    const offset = row * 8;
    //    setCodePoint(block, offset, codePoint);

    const isAlpha = alphabetic.isInRange(codePoint);
    const isUpper = uppercase.isInRange(codePoint);
    const isLower = lowercase.isInRange(codePoint);
    const isWhitespace = whitespace.isInRange(codePoint);

    const category = parts[2];
    const isPrint =
      !category.startsWith("M") &&
      !category.startsWith("Z") &&
      !category.startsWith("C");

    block[offset] =
      (isAlpha ? 0x01 : 0) |
      (isUpper ? 0x02 : 0) |
      (isLower ? 0x04 : 0) |
      (isWhitespace ? 0x08 : 0) |
      (isPrint ? 0x10 : 0);

    if (parts[6].length) {
      block[offset + 1] = parseInt(parts[6]);
    } else if (parts[7].length && parts[7] === parts[8]) {
      block[offset + 1] = parseInt(parts[7]);
    } else {
      block[offset + 1] = 255;
    }

    const upper = parts[12];
    if (upper.length) {
      const upperCodePoint = parseInt(upper, 16);
      setCodePoint(block, offset + 2, upperCodePoint);
    }
    const lower = parts[13];
    if (lower.length) {
      const lowerCodePoint = parseInt(lower, 16);
      setCodePoint(block, offset + 5, lowerCodePoint);
    }
  }
  await writeBlock(blockIndex, block);
  blocks[blockIndex] = Buffer.from(block).toString("base64");
  await writeBlockList(blocks);
  const end = performance.now();
  const duration = (end - start) / 1000;
  console.log(`Processed ${count} code points, in ${duration.toFixed(1)}s`);
}

main();
