import * as fs from "fs";

function decodePacket(input: string): Uint8Array {
  const buffer = new Uint8Array(input.length / 2);
  for (let i = 0; i < input.length; i += 2) {
    const hex = input.slice(i, i + 2);
    buffer[i / 2] = parseInt(hex, 16);
  }
  return buffer;
}

interface ParseState {
  buffer: Uint8Array;
  bitIndex: number;
}

function readBits(count: number, state: ParseState): number {
  let value = 0;
  for (let i = 0; i < count; i++) {
    const bit = (state.buffer[Math.trunc(state.bitIndex / 8)] >> (7 - (state.bitIndex % 8))) & 1;
    value |= (bit << (count - 1 - i));
    state.bitIndex++;
  }
  return value;
}

function parsePacket(buffer: Uint8Array) {
  const state: ParseState = { buffer, bitIndex: 0 };
  const version = readBits(3, state);
  const type = readBits(3, state);
  console.log(`Version: ${version}`);
  console.log(`Type: ${type}`);
}

async function main() {
  const input = await fs.promises.readFile("resources/literal-demo.txt", { encoding: "utf-8" });
  const rawPacket = decodePacket(input);
  const packet = parsePacket(rawPacket);
}

main();
