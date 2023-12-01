import { getPuzzleInput } from "../utils.js";

const part1 = (lines: string[]) => {
  const digits = lines.map((line) => line.replace(/\D/g, ""));

  const calibrationValues = digits.map((line) => {
    const first = line.at(0);
    const last = line.at(line.length - 1);

    return Number.parseInt(`${first}${last}`);
  });

  const total = calibrationValues.reduce((acc, curr) => acc + curr, 0);

  console.log(`Part 1: ${total}`);
};

const trebuchet = async () => {
  const puzzleInput = await getPuzzleInput(import.meta.url);
  const lines = puzzleInput.split("\n").map((line) => line.trim());

  part1(lines);
};

await trebuchet();
