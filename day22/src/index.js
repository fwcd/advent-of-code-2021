import * as fs from 'fs';

function intersectIntervals(iv1, iv2) {
  if (iv1.from <= iv2.to && iv2.from <= iv1.to) {
    return { from: Math.max(iv1.from, iv2.from), to: Math.min(iv1.to, iv2.to) };
  } else {
    return null;
  }
}

function inInterval(iv, x) {
  return iv.from <= x && x <= iv.to;
}

function sliceUpIntervals(iv1, iv2) {
  const it = intersectIntervals(iv1, iv2);
  if (it) {
    return [
      { from: Math.min(iv1.from, iv2.from), to: it.from - 1 },
      it,
      { from: it.to + 1, to: Math.max(iv1.to, iv2.to) }
    ];
  } else {
    return null;
  }
}

function subtractIntervals(iv1, iv2) {
  const slices = sliceUpIntervals(iv1, iv2);
  if (slices) {
    return slices.filter(iv => iv.from < iv.to);
  } else {
    return [iv1];
  }
}

function appendCuboid(ivs, c) {
  let iv = iv2;
  for (const iv1 of ivs) {
    if (intersectCuboids(iv1, iv2)) {
      iv = joinIntervals(iv, iv1);
    }
  }
  return ivs.filter(iv1 => !intersectIntervals(iv1, iv2)) + [iv];
}

function inCuboid(c, v) {
  return inInterval(c.x, v.x) && inInterval(c.y, v.y) && inInterval(c.z, v.z);
}
function subtractCuboids(c1, c2) {
  const cs = [];
  for (const x of subtractIntervals(c1.x, c2.x)) {
    for (const y of subtractIntervals(c1.y, c2.y)) {
      for (const z of subtractIntervals(c1.z, c2.z)) {
        cs.push({ x, y, z });
      }
    }
  }
  return cs;
}

async function main() {
  const input = await fs.promises.readFile('resources/input.txt', { encoding: 'utf-8' });
  const instructions = input
    .split('\n')
    .filter(l => l.length > 0)
    .map(l => [.../(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/.exec(l)].slice(1))
    .map((([state, x1, x2, y1, y2, z1, z2]) => ({
      on: state === 'on',
      x: { from: x1, to: x2 },
      y: { from: y1, to: y2 },
      z: { from: z1, to: z2 }
    })));
  const cuboids = [];

  for (const inst of instructions) {
    if (inst.on) {
      cuboids = appendCuboid(cuboids, inst);
    } else {
      cuboids = cuboids.flatMap(c => subtractCuboids(c, inst));
    }
  }

  // let part1 = 0;
  // for (let x = -50; x <= 50; x++) {
  //   for (let y = -50; y <= 50; y++) {
  //     for (let z = -50; z <= 50; z++) {
  //       let on = false;
  //       for (const cuboid of instructions) {
  //         if (inCuboid(cuboid, { x, y, z })) {
  //           on = cuboid.on;
  //         }
  //       }
  //       part1 += on ? 1 : 0;
  //     }
  //   }
  // }

  // console.log(`Part 1: ${part1}`);
}

main();
