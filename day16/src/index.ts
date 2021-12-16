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
  version: number;
}

interface LiteralPacket extends BasePacket {
  type: "literal";
  value: number;
}

interface OperatorPacket extends BasePacket {
  type: "operator";
  subPackets: Packet[];
  operator: (xs: number[]) => number;
}

type Packet = LiteralPacket | OperatorPacket;

function parsePacket(state: ParseState): Packet {
  const version = readBits(3, state);
  const type = readBits(3, state);
  const base: BasePacket = { version };

  switch (type) {
  case 4: // Literal
    let value = 0;
    let lastBit = 1;
    while (lastBit === 1) {
      lastBit = nextBit(state);
      const group = readBits(5, state);
      value = (value << 4) | (group & 0b1111);
    }
    return { type: "literal", value, ...base };
  default: // Operator
    const lengthTypeId = readBits(1, state);
    const subPackets: Packet[] = [];

    if (lengthTypeId === 0) {
      const totalLength = readBits(15, state);
      const startBitIndex = state.bitIndex;
      while (state.bitIndex < startBitIndex + totalLength) {
        subPackets.push(parsePacket(state));
      }
    } else {
      const subPacketCount = readBits(11, state);
      for (let i = 0; i < subPacketCount; i++) {
        subPackets.push(parsePacket(state));
      }
    }

    let operator: (xs: number[]) => number;
    switch (type) {
    case 0: operator = xs => xs.reduce((a, b) => a + b); break;
    case 1: operator = xs => xs.reduce((a, b) => a * b); break;
    case 2: operator = xs => Math.min(...xs); break;
    case 3: operator = xs => Math.max(...xs); break;
    case 5: operator = xs => xs[0] > xs[1] ? 1 : 0; break;
    case 6: operator = xs => xs[0] < xs[1] ? 1 : 0; break;
    case 7: operator = xs => xs[0] === xs[1] ? 1 : 0; break;
    default: break;
    }

    return { type: "operator", operator, subPackets, ...base }
  }
}

function sumVersions(packet: Packet): number {
  let sum = packet.version;
  if (packet.type === "operator") {
    sum += packet.subPackets.map(p => sumVersions(p)).reduce((a, b) => a + b);
  }
  return sum;
}

function evaluate(packet: Packet): number {
  switch (packet.type) {
  case "literal":
    return packet.value;
  case "operator":
    return packet.operator(packet.subPackets.map(p => evaluate(p)));
  }
}

async function main() {
  const input = await fs.promises.readFile("resources/input.txt", { encoding: "utf-8" });
  const rawPacket = decodePacket(input);
  const packet = parsePacket({ buffer: rawPacket, bitIndex: 0 });

  console.log(JSON.stringify(packet, null, 2));
  console.log(`Part 1: ${sumVersions(packet)}`);
  console.log(`Part 2: ${evaluate(packet)}`);
}

main();
