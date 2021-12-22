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

function boundInIntervalIntersection(iv, it) {
  if (inInterval(iv, it.from)) {
    return it.from;
  } else {
    return it.to;
  }
}

function cornerInCuboidIntersection(c, it) {
  return {
    x: boundInIntervalIntersection(it.x, c.x),
    y: boundInIntervalIntersection(it.y, c.y),
    z: boundInIntervalIntersection(it.z, c.z)
  };
}

function equalIntervals(iv1, iv2) {
  return iv1.from === iv2.from && iv1.to === iv2.to;
}

function equalCuboids(c1, c2) {
  return equalIntervals(c1.x, c2.x) && equalIntervals(c1.y, c2.y) && equalIntervals(c1.z, c2.z);
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

function sliceCuboid(c, v, dir) { // into <= 8 pieces
  const cs = [];
  for (const x in [{ from: c.x.from, to: v.x - dir.x }, { from: v.x + (1 - dir.x), to: c.x.to }]) {
    if (x.from < x.to) {
      for (const y in [{ from: c.y.from, to: v.y - dir.y }, { from: v.y + (1 - dir.y), to: c.y.to }]) {
        if (y.from < y.to) {
          for (const z in [{ from: c.z.from, to: v.z - dir.z }, { from: v.z + (1 - dir.z), to: c.z.to }]) {
            if (z.from < z.to) {
              cs.push({ x, y, z });
            }
          }
        }
      }
    }
  }
  return cs;
}

function combineCuboids(c1, c2, additive) {
  const it = intersectCuboids(c1, c2);
  if (it) {
    const dir = {
      x: it.x.to < c1.x.to,
      y: it.y.to < c1.y.to,
      z: it.z.to < c1.z.to
    };
    let cs = sliceCuboid(c1, cornerInCuboidIntersection(c1, it), dir);
    if (additive) {
      let cs2 = sliceCuboid(c2, cornerInCuboidIntersection(c2, it), { x: -dir.x, y: -dir.y, z: -dir.z })
        .filter(c => !equalCuboids(c, it));
      cs.push(...cs2);
    } else {
      cs = cs.filter(c => !equalCuboids(c, it));
    }
    return cs;
  } else {
    return additive ? [c1, c2] : [c1];
  }
}

function combineIntoCuboids(cs, c2, additive) {
  if (additive) {
    const newCS = [...cs, c2];
    merger:
    while (true) {
      for (let i = 0; i < newCS.length; i++) {
        for (let j = i + 1; j < newCS.length; j++) {
          const lhs = newCS[i];
          const rhs = newCS[j];
          // if (intersectCuboids(lhs, rhs)) {
          //   newCS.splice(i);
          //   newCS.splice(j);
          //   newCS.push(...combineCuboids(lhs, rhs, true));
          //   continue merger;
          // }
        }
      }
      break;
    }
    return newCS;
  } else {
    return cs.flatMap(c => combineCuboids(c, c2, false));
  }
}

async function main() {
  const input = await fs.promises.readFile('resources/demo.txt', { encoding: 'utf-8' });
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
  let cuboids = [];

  for (const inst of instructions) {
    cuboids = combineIntoCuboids(cuboids, inst, inst.on);
  }

  const part2 = cuboids.reduce((sum, c) => sum + cuboidVolume(c), 0);
  console.log(`Part 2: ${part2}`);

  let part1 = 0;
  for (let x = -50; x <= 50; x++) {
    for (let y = -50; y <= 50; y++) {
      for (let z = -50; z <= 50; z++) {
        let on = false;
        for (const cuboid of instructions) {
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
