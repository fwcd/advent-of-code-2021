import * as fs from 'fs';

function inInterval(iv, x) {
  return iv.from <= x && x <= iv.to;
}

function inCuboid(c, v) {
  return inInterval(c.x, v.x) && inInterval(c.y, v.y) && inInterval(c.z, v.z);
}

function intervalLength(iv) {
  return iv.to - iv.from + 1;
}

function cuboidVolume(c) {
  return intervalLength(c.x) * intervalLength(c.y) * intervalLength(c.z);
}

function intersectIntervals(iv1, iv2) {
  const it = { from: Math.max(iv1.from, iv2.from), to: Math.min(iv1.to, iv2.to) };
  return it.from <= it.to ? it : null;
}

function intersectCuboids(iv1, iv2) {
  const x = intersectIntervals(iv1.x, iv2.x);
  const y = intersectIntervals(iv1.y, iv2.y);
  const z = intersectIntervals(iv1.z, iv2.z);
  return x && y && z ? { x, y, z } : null;
}

function encodeCuboid(c) {
  return JSON.stringify([c.x.from, c.x.to, c.y.from, c.y.to, c.z.from, c.z.to]);
}

function decodeCuboid(raw) {
  const [x1, x2, y1, y2, z1, z2] = JSON.parse(raw);
  return {
    x: { from: x1, to: x2 },
    y: { from: y1, to: y2 },
    z: { from: z1, to: z2 }
  };
}

function computeTotalVolume(instructions, boundingBox) {
  // Use clever signed volume trick by Boojam, instead of computing
  // cuboid slices just add the intersection with negative volume.

  let cuboidCounts = {};
  for (const inst of instructions) {
    const delta = [];
    for (const otherRaw in cuboidCounts) {
      let it = intersectCuboids(inst.cuboid, decodeCuboid(otherRaw));
      if (it && boundingBox) {
        it = intersectCuboids(it, boundingBox);
      }
      if (it) {
        delta.push({ cuboid: it, sign: -cuboidCounts[otherRaw] });
      }
    }
    if (inst.on) {
      let it = inst.cuboid;
      if (boundingBox) {
        it = intersectCuboids(it, boundingBox);
      }
      if (it) {
        delta.push({ cuboid: it, sign: 1 });
      }
    }
    for (const volumeCuboid of delta) {
      const raw = encodeCuboid(volumeCuboid.cuboid);
      cuboidCounts[raw] = (raw in cuboidCounts ? cuboidCounts[raw] : 0) + volumeCuboid.sign;
    }
  }

  return Object.keys(cuboidCounts)
    .map(raw => [decodeCuboid(raw), cuboidCounts[raw]])
    .reduce((sum, [c, s]) => sum + cuboidVolume(c) * s, 0);
}

async function main() {
  const input = await fs.promises.readFile('resources/input.txt', { encoding: 'utf-8' });
  const instructions = input
    .split('\n')
    .filter(l => l.length > 0)
    .map(l => [.../(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/.exec(l)].slice(1))
    .map((([state, x1, x2, y1, y2, z1, z2]) => ({
      on: state === 'on',
      cuboid: {
        x: { from: parseInt(x1), to: parseInt(x2) },
        y: { from: parseInt(y1), to: parseInt(y2) },
        z: { from: parseInt(z1), to: parseInt(z2) }
      }
    })));

  const part1 = computeTotalVolume(instructions, {
    x: { from: -50, to: 50 },
    y: { from: -50, to: 50 },
    z: { from: -50, to: 50 }
  });
  console.log(`Part 1: ${part1}`);

  const part2 = computeTotalVolume(instructions, null);
  console.log(`Part 2: ${part2}`);
}

main();
