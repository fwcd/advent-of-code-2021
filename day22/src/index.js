import * as fs from 'fs';

async function main() {
  const input = await fs.promises.readFile('resources/input.txt', { encoding: 'utf-8' });
  const cubes = input
    .split('\n')
    .filter(l => l.length > 0)
    .map(l => [.../(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/.exec(l)].slice(1));

  let part1 = 0;
  for (let x = -50; x <= 50; x++) {
    for (let y = -50; y <= 50; y++) {
      for (let z = -50; z <= 50; z++) {
        let on = false;
        for (const [state, x1, x2, y1, y2, z1, z2] of cubes) {
          if (x1 <= x && x <= x2 && y1 <= y && y <= y2 && z1 <= z && z <= z2) {
            on = state === 'on';
          }
        }
        part1 += on ? 1 : 0;
      }
    }
  }

  console.log(`Part 1: ${part1}`);
}

main();
