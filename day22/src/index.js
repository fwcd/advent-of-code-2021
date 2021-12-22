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
  if (iv1.from <= iv2.to && iv2.from <= iv1.to) {
    return { from: Math.max(iv1.from, iv2.from), to: Math.min(iv1.to, iv2.to) };
  } else {
    return null;
  }
}

function intersectCuboids(iv1, iv2) {
  const x = intersectIntervals(iv1.x, iv2.x);
  const y = intersectIntervals(iv1.y, iv2.y);
  const z = intersectIntervals(iv1.z, iv2.z);
  if (x && y && z) {
    return { x, y, z };
  } else {
    return null;
  }
}

async function main() {
  const input = await fs.promises.readFile('resources/demo-large.txt', { encoding: 'utf-8' });
  const instructions = input
    .split('\n')
    .filter(l => l.length > 0)
    .map(l => [.../(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/.exec(l)].slice(1))
    .map((([state, x1, x2, y1, y2, z1, z2]) => ({
      on: state === 'on',
      cuboid: {
        x: { from: x1, to: x2 },
        y: { from: y1, to: y2 },
        z: { from: z1, to: z2 }
      }
    })));
  let cuboids = [];

  for (const inst of instructions) {
    const existing = cuboids.length;
    for (let i = 0; i < existing; i++) {
      const it = intersectCuboids(inst.cuboid, cuboids[i]);
      if (it) {
        cuboids.push({ ...it, sign: -1 });
      }
    }
    if (inst.on) {
      cuboids.push({ ...inst.cuboid, sign: 1 });
    }
  }

  const part2 = cuboids.reduce((sum, c) => sum + cuboidVolume(c) * c.sign, 0);
  console.log(`Part 2: ${part2}`);

  let part1 = 0;
  for (let x = -50; x <= 50; x++) {
    for (let y = -50; y <= 50; y++) {
      for (let z = -50; z <= 50; z++) {
        let on = false;
        for (const inst of instructions) {
          const cuboid = inst.cuboid;
          if (inCuboid(cuboid, { x, y, z })) {
            on = cuboid.on;
          }
        }
        part1 += on ? 1 : 0;
      }
    }
  }

  console.log(`Part 1: ${part1}`);
}

main();
