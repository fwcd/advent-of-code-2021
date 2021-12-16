import * as fs from "fs";

async function main() {
  const input = await fs.promises.readFile("resources/demo.txt", { encoding: "utf-8" });
  console.log(input);
}

main();
