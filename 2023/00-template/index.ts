import { getPuzzleInput, logChallenge } from "../utils.js";

const template = async () => {
  const puzzleInput = getPuzzleInput(import.meta.url);

  logChallenge({
    name: "Day 0: Template",
    part1: undefined,
    part2: undefined,
  });
};

await template();
