import chalk from "chalk";
import gradient from "gradient-string";
import { readFileSync } from "node:fs";
import path from "path";
import { fileURLToPath } from "url";

type Answer = string | number | void;

type Part = {
  run: (() => Answer) | (() => Promise<Answer>);
  expected?: string | number;
};

export const getPuzzleInput = (
  moduleUrl: string,
  { useExample }: { useExample?: boolean } = {},
) => {
  const fileName = `${useExample ? "example" : "puzzle"}-input.txt`;
  const dirPath = path.dirname(fileURLToPath(moduleUrl));

  const filePath = path.join(dirPath, fileName);

  return readFileSync(filePath).toString().trim();
};

export const toLines = (fileContents: string) => fileContents.split("\n").map((l) => l.trim());

export const xmasGradient = gradient("red", "green");

const border = () => xmasGradient("-".repeat(process.stdout.columns));

const part = async (number: 1 | 2, part: Part | undefined) => {
  if (!part) {
    return chalk.dim(`Part ${number}: TODO`);
  }

  const { run, expected } = part;
  const start = performance.now();
  const result = await run();
  const timeTaken = performance.now() - start;

  const correct = result?.toString() === expected?.toString();

  const partOutput = `${
    expected ? `${correct ? chalk.green("✔️") : chalk.red("x")} ` : ""
  }${`Part ${number}:`} ${chalk.green(
    result
      ? correct
        ? chalk.green(result)
        : !!expected
          ? chalk.red(result)
          : chalk.yellow(result)
      : chalk.yellow("TODO"),
  )}`;
  const timeTakenMessage = chalk.dim(`${timeTaken.toFixed(2)}ms`);
  const expectedMessage = expected && !correct ? chalk.dim(`\nExpected > ${expected}`) : "";

  return partOutput + chalk.dim(" | ") + timeTakenMessage + expectedMessage;
};

export const runPuzzle = async ({
  day,
  name,
  part1,
  part2,
}: {
  day: number;
  name: string;
  part1?: Part;
  part2?: Part;
}) => {
  console.log();
  console.log(xmasGradient(`🎄 Day ${day}: ${name}`));
  console.log(border());

  const p1 = await part(1, part1);
  console.log(p1);
  console.log(border());

  const p2 = await part(2, part2);
  console.log(p2);
  console.log(border());

  console.log();
};

export const transpose = <T>(array: T[][]) => {
  return array[0]!.map((_, colIndex) => array.map((row) => row[colIndex]!));
};

export const wait = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));
