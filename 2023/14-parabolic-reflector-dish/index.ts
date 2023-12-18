import { getPuzzleInput, runPuzzle, toLines, transpose } from "../utils.js";

const useExample = false;

const symbol = {
  round: "O",
  cube: "#",
  empty: ".",
} as const;

const notRoundRegex = new RegExp(`[^${symbol.round}]`, "g");

const platform = toLines(getPuzzleInput(import.meta.url, { useExample }));

const platformTranspose = (platform: string[]) =>
  transpose(platform.map((row) => row.split(""))).map((column) => column.join(""));

const tiltPlatform = (platform: string[], direction: "west" | "east") =>
  platform.map((row) =>
    row
      .split(symbol.cube)
      .map((symbols) => {
        const numRound = symbols.replaceAll(notRoundRegex, "").length;
        const rounds = symbol.round.repeat(numRound);
        const empties = symbol.empty.repeat(symbols.length - numRound);
        return direction === "west" ? rounds + empties : empties + rounds;
      })
      .join(symbol.cube),
  );

const tiltWest = (platform: string[]) => tiltPlatform(platform, "west");
const tiltNorth = (platform: string[]) => platformTranspose(tiltWest(platformTranspose(platform)));
const tiltEast = (platform: string[]) => tiltPlatform(platform, "east");
const tiltSouth = (platform: string[]) => platformTranspose(tiltEast(platformTranspose(platform)));

const cycle = (platform: string[]) => tiltEast(tiltSouth(tiltWest(tiltNorth(platform))));

const northLoad = (platform: string[]) =>
  platform
    .reverse()
    .reduce(
      (prev, curr, index) => prev + curr.replaceAll(notRoundRegex, "").length * (index + 1),
      0,
    );

const part1 = () => northLoad(tiltNorth(platform));

const part2 = () => {
  const cyclesToPerform = 1_000_000_000;
  const cache = new Map<string, { next: string[]; cycle: number }>();

  let p = platform;
  let c = 0;

  for (; c < cyclesToPerform; c++) {
    const cacheKey = p.join();

    if (cache.has(cacheKey)) {
      break;
    } else {
      const next = cycle(p);
      cache.set(cacheKey, { next, cycle: c });
      p = next;
    }
  }

  const cyclesRemaining = cyclesToPerform - c;
  const repeatedCycleLength = c - cache.get(p.join())!.cycle;

  for (let i = 0; i < cyclesRemaining % repeatedCycleLength; i++) {
    p = cache.get(p.join())!.next;
  }

  return northLoad(p);
};

/**
 * @description https://adventofcode.com/2023/day/14
 */
runPuzzle({
  day: 14,
  name: "Parabolic Reflector Dish",
  part1: { run: part1, expected: useExample ? 136 : 105003 },
  part2: { run: part2, expected: useExample ? 64 : 93742 },
});
