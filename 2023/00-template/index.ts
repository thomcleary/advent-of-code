import { getPuzzleInput, logChallenge } from "../utils.js";

const puzzleInput = getPuzzleInput(import.meta.url);

const part1 = () => {};

const part2 = () => {};

const template = () =>
  logChallenge({
    name: "Day 0: Template",
    part1: { run: part1, expected: undefined },
    part2: { run: part2, expected: undefined },
  });

template();
