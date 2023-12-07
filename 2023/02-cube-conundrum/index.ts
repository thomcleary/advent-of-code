import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

type Game = {
  id: number;
  sets: [cubeCount: number, cubeColour: keyof typeof bag][][];
};
type CubeCounts = [red: number, green: number, blue: number];

const bag = { red: 12, green: 13, blue: 14 } as const;

const games = toLines(getPuzzleInput(import.meta.url))
  .map((line) => line.split(":") as [string, string])
  .map(([id, sets]) => ({
    id: Number.parseInt(id.replaceAll(/\D/g, "")),
    sets: sets.split(";").map((set) =>
      set
        .split(",")
        .map((cubeCount) => cubeCount.trim().split(" ") as [string, keyof typeof bag])
        .map(([count, colour]) => [Number.parseInt(count), colour]),
    ),
  })) satisfies Game[];

const part1 = () =>
  games
    .map((game) => ({
      ...game,
      valid: game.sets
        .map((set) => set.reduce((prev, [cubeCount, cubeColour]) => prev && cubeCount <= bag[cubeColour], true))
        .every((isSetValid) => isSetValid),
    }))
    .reduce((prev, { id, valid }) => (valid ? id + prev : prev), 0);

const part2 = () =>
  games
    .map((game) => ({
      ...game,
      power: game.sets
        .map((set) =>
          set.reduce((prev, [cubeCount, cubeColour]) => ({ ...prev, [cubeColour]: cubeCount }), {
            red: 0,
            green: 0,
            blue: 0,
          }),
        )
        .reduce(
          ([prevRed, prevGreen, prevBlue], { red, green, blue }) =>
            [Math.max(prevRed, red), Math.max(prevGreen, green), Math.max(prevBlue, blue)] satisfies CubeCounts,
          [0, 0, 0] satisfies CubeCounts,
        )
        .reduce((prev, curr) => prev * curr),
    }))
    .reduce((prev, { power }) => prev + power, 0);

const cubeConundrum = () =>
  logChallenge({
    name: "Day 2: Cube Conundrum",
    part1: { run: part1, expected: 2685 },
    part2: { run: part2, expected: 83707 },
  });

cubeConundrum();
