import { getPuzzleInput, runPuzzle, transpose } from "../utils.js";

const useExample = false;

type WithSmudge = { withSmudge?: boolean };

const terrain = {
  ash: ".",
  rock: "#",
} as const;

const patterns = getPuzzleInput(import.meta.url, { useExample })
  .split("\n\n")
  .map((pattern) => pattern.split("\n"))
  .map((p) => ({
    row: p,
    column: transpose(p.map((row) => row.split(""))).map((row) => row.join("")),
  }));

const hasSmudge = (top: string, bottom: string) =>
  top
    .split("")
    .map((t, i) => t === bottom.charAt(i))
    .filter((match) => !match).length == 1;

const findMatches = (pattern: string[], { withSmudge }: WithSmudge = {}) => {
  let top = 0;
  let bottom = 1;
  let matches: [number, number][] = [];

  while (pattern[bottom] !== undefined) {
    const [topRow, bottomRow] = [pattern[top]!, pattern[bottom]!];
    if (topRow === bottomRow || (withSmudge && hasSmudge(topRow, bottomRow))) {
      matches.push([top, bottom]);
    }
    top++;
    bottom++;
  }

  return matches;
};

const reflectionScore = (pattern: string[], { withSmudge }: WithSmudge = {}) => {
  for (const match of findMatches(pattern, { withSmudge })) {
    let [top, bottom] = match;
    let smudgeFound = false;
    let reflectionFound = true;

    while (pattern[top] !== undefined && pattern[bottom] !== undefined) {
      const [topRow, bottomRow] = [pattern[top]!, pattern[bottom]!];
      if (topRow !== bottomRow) {
        smudgeFound = !!withSmudge && hasSmudge(topRow, bottomRow);
        if (!withSmudge || !smudgeFound) {
          reflectionFound = false;
          break;
        }
      }
      top--;
      bottom++;
    }

    reflectionFound = !withSmudge ? reflectionFound : smudgeFound && reflectionFound;

    if (reflectionFound) {
      return match[0] + 1;
    }
  }

  return undefined;
};

const reflectionSum = ({ withSmudge }: WithSmudge = {}) =>
  patterns
    .map((pattern) => {
      const rowScore = reflectionScore(pattern.row, { withSmudge });
      return rowScore ? rowScore * 100 : reflectionScore(pattern.column, { withSmudge })!;
    })
    .reduce((p, c) => p + c, 0);

const part1 = () => reflectionSum();

const part2 = () => reflectionSum({ withSmudge: true });

/**
 * @description https://adventofcode.com/2023/day/13
 */
runPuzzle({
  day: 13,
  name: "Point of Incidence",
  part1: { run: part1, expected: useExample ? 405 : 35521 },
  part2: { run: part2, expected: useExample ? 400 : 34795 },
});
