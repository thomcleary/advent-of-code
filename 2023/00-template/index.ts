import { getPuzzleInput, logChallenge } from "../utils.js";

const useExample = true;

const puzzleInput = getPuzzleInput(import.meta.url, { useExample });

const part1 = () => {};

const part2 = () => {};

const template = () =>
  logChallenge({
    name: "Day 0: Template",
    part1: { run: part1, expected: useExample ? undefined : undefined },
    part2: { run: part2, expected: useExample ? undefined : undefined },
  });

template();
