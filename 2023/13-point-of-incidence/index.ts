import { getPuzzleInput, logChallenge, transpose } from "../utils.js";

const useExample = false;

const patterns = getPuzzleInput(import.meta.url, { useExample })
  .split("\n\n")
  .map((pattern) => pattern.split("\n"))
  .map((p) => ({
    row: p,
    column: transpose(p.map((row) => row.split(""))).map((row) => row.join("")),
  }));

const findMatches = (pattern: string[]) => {
  let top = 0;
  let bottom = 1;
  let matches: [number, number][] = [];

  while (pattern[bottom] !== undefined) {
    pattern[top] === pattern[bottom] && matches.push([top, bottom]);
    top++;
    bottom++;
  }

  return matches;
};

const reflectionScore = (pattern: string[]) => {
  for (const match of findMatches(pattern)) {
    let [top, bottom] = match;
    let reflectionFound = true;

    while (pattern[top] !== undefined && pattern[bottom] !== undefined) {
      if (pattern[top] !== pattern[bottom]) {
        reflectionFound = false;
        break;
      }
      top--;
      bottom++;
    }

    if (reflectionFound) {
      return match[0] + 1;
    }
  }

  return undefined;
};

const part1 = () =>
  patterns
    .map((pattern) => {
      const rowScore = reflectionScore(pattern.row);
      return rowScore ? rowScore * 100 : reflectionScore(pattern.column)!;
    })
    .reduce((p, c) => p + c, 0);

const part2 = () => {};

const pointOfIncidence = () =>
  logChallenge({
    name: "Day 13: Point of Incidence",
    part1: { run: part1, expected: useExample ? 405 : 35521 },
    part2: { run: part2, expected: undefined },
  });

pointOfIncidence();
