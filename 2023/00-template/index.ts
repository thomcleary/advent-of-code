import { getPuzzleInput, runPuzzle } from "../utils.js";

const useExample = true;

const puzzleInput = getPuzzleInput(import.meta.url, { useExample });

const part1 = () => {};

const part2 = () => {};

/**
 * @description https://adventofcode.com/2023/day/0
 */
runPuzzle({
  day: 0,
  name: "Template",
  part1: { run: part1, expected: useExample ? undefined : undefined },
  part2: { run: part2, expected: useExample ? undefined : undefined },
});
