import * as fs from 'fs';

function intersectIntervals(iv1, iv2) {
  if (iv1.from <= iv2.to && iv2.from <= iv1.to) {
    return { from: Math.max(iv1.from, iv2.from), to: Math.min(iv1.to, iv2.to) };
  } else {
    return null;
  }
}

function inInterval(x, iv) {
  return iv.from <= x && x <= iv.to;
}

function inCuboid(v, c) {
  return inInterval(v.x, c.x) && inInterval(v.y, c.y) && inInterval(v.z, c.z);
}

async function main() {
  const input = await fs.promises.readFile('resources/input.txt', { encoding: 'utf-8' });
  const cuboids = input
    .split('\n')
    .filter(l => l.length > 0)
    .map(l => [.../(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/.exec(l)].slice(1))
    .map((([state, x1, x2, y1, y2, z1, z2]) => ({
      on: state === 'on',
      x: { from: x1, to: x2 },
      y: { from: y1, to: y2 },
      z: { from: z1, to: z2 }
    })));

  let part1 = 0;
  for (let x = -50; x <= 50; x++) {
    for (let y = -50; y <= 50; y++) {
      for (let z = -50; z <= 50; z++) {
        let on = false;
        for (const cuboid of cuboids) {
          if (inCuboid({ x, y, z }, cuboid)) {
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
