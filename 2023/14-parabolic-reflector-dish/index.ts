import { getPuzzleInput, logChallenge, toLines, transpose } from "../utils.js";

const useExample = false;

const symbol = {
  round: "O",
  cube: "#",
  empty: ".",
} as const;

const platform = toLines(getPuzzleInput(import.meta.url, { useExample }));

const platformTranspose = (platform: string[]) =>
  transpose(platform.map((row) => row.split(""))).map((column) => column.join(""));

const tiltNorth = (platform: string[]) => {
  const pTranspose = platformTranspose(platform);
  pTranspose.forEach((column, index) => {
    const sections = column.split(symbol.cube).map((symbols) => symbols.split(""));
    sections.forEach((section) => section.sort((a, b) => (a === b ? 0 : b === symbol.round ? 1 : -1)));
    pTranspose[index] = sections.map((s) => s.join("")).join(symbol.cube);
  });
  return platformTranspose(pTranspose);
};

const part1 = () =>
  tiltNorth(platform)
    .reverse()
    .reduce(
      (prev, curr, index) => prev + curr.replaceAll(new RegExp(`[^${symbol.round}]`, "g"), "").length * (index + 1),
      0,
    );

const part2 = () => {};

const parabolicReflectorDish = () =>
  logChallenge({
    name: "Day 14: Parabolic Reflector Dish",
    part1: { run: part1, expected: useExample ? 136 : 105003 },
    part2: { run: part2, expected: undefined },
  });

parabolicReflectorDish();
