import chalk from "chalk";
import gradient from "gradient-string";
import { readFile } from "node:fs/promises";
import path from "path";
import { fileURLToPath } from "url";

type Part = {
  run: () => string | number | void;
  expected?: string | number;
};

export const getPuzzleInput = async (moduleUrl: string) => {
  const fileName = "puzzle-input.txt";
  const dirPath = path.dirname(fileURLToPath(moduleUrl));

  const filePath = path.join(dirPath, fileName);

  return (await readFile(filePath)).toString().trim();
};

export const toLines = (fileContents: string) => fileContents.split("\n").map((l) => l.trim());

const border = () => gradient("red", "green")("-".repeat(process.stdout.columns));

const part = (number: 1 | 2, part: Part | undefined) => {
  if (!part) {
    return chalk.dim(`Part ${number}: TODO`);
  }

  const { run, expected } = part;
  const start = performance.now();
  const result = run();
  const timeTaken = performance.now() - start;

  const correct = result?.toString() === expected?.toString();

  const partOutput = `${expected ? `${correct ? "✅" : "❌"} ` : ""}${chalk.red(`Part ${number}:`)} ${chalk.green(
    result ?? "TODO",
  )}`;
  const timeTakenMessage = chalk.dim(`${timeTaken.toFixed(2)}ms`);
  const expectedMessage = expected && !correct ? chalk.dim(`\nExpected > ${expected}`) : "";

  return partOutput + chalk.dim(" | ") + timeTakenMessage + expectedMessage;
};

export const logChallenge = ({ name, part1, part2 }: { name: string; part1?: Part; part2?: Part }) => {
  console.log(border());
  console.log(`🎄 ${name}`);
  console.log(border());
  console.log(part(1, part1));
  console.log(border());
  console.log(part(2, part2));
  console.log(border());
};
