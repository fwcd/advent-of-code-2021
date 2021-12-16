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

function nextBit(state: ParseState): number {
  return (state.buffer[Math.trunc(state.bitIndex / 8)] >> (7 - (state.bitIndex % 8))) & 1;
}

function readBits(count: number, state: ParseState): number {
  let value = 0;
  for (let i = 0; i < count; i++) {
    const bit = nextBit(state);
    value |= (bit << (count - 1 - i));
    state.bitIndex++;
  }
  return value;
}

interface BasePacket {
  type: string;
  version: number;
}

interface LiteralPacket extends BasePacket {
  type: "literal";
  value: number;
}

type Packet = LiteralPacket;

function parsePacket(buffer: Uint8Array): Packet {
  const state: ParseState = { buffer, bitIndex: 0 };
  const version = readBits(3, state);
  const type = readBits(3, state);
  switch (type) {
  case 4: // Literal
    let value = 0;
    let lastBit = 1;
    while (lastBit === 1) {
      lastBit = nextBit(state);
      const group = readBits(5, state);
      value = (value << 4) | (group & 0b1111);
    }
    return { type: "literal", version, value };
  default:
    break;
  }
}

async function main() {
  const input = await fs.promises.readFile("resources/literal-demo.txt", { encoding: "utf-8" });
  const rawPacket = decodePacket(input);
  const packet = parsePacket(rawPacket);
  console.log(JSON.stringify(packet));
}

main();
