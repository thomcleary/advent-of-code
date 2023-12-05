import { getPuzzleInput, logChallenge } from "../utils.js";

type AlmanacMaps = Record<string, CategoryMap>;
type CategoryMap = { to: string; ranges: Range[] };
type Range = { destStart: number; sourceStart: number; length: number };

const almanac = (await getPuzzleInput(import.meta.url)).split("\n\n");

const seeds = almanac[0]
  ?.split(":")[1]
  ?.trim()
  ?.split(" ")
  .map((seed) => Number.parseInt(seed))!;

const maps = almanac
  .slice(1)
  .map((m) => m.split(" map:\n") as [string, string])
  .reduce<AlmanacMaps>((prev, [mapName, ranges]) => {
    const [from, to] = mapName.split("-to-") as [string, string];
    return {
      ...prev,
      [from]: {
        to,
        ranges: ranges.split("\n").map((r) => {
          const [destStart, sourceStart, length] = r.split(" ").map((n) => Number.parseInt(n));
          return { destStart, sourceStart, length } as Range;
        }),
      },
    };
  }, {});

const locationForSeed = (seed: number) => {
  let source: string | undefined = "seed";
  let number = seed;

  while (source && source !== "location") {
    const dest: CategoryMap | undefined = maps[source]; // https://github.com/microsoft/TypeScript/issues/43047
    for (const { destStart, sourceStart, length } of dest?.ranges ?? []) {
      const numberIsInRange = number >= sourceStart && number <= sourceStart + length;
      if (numberIsInRange) {
        number = destStart + (number - sourceStart);
        break;
      }
    }
    source = dest?.to;
  }

  return number;
};

const part1 = () =>
  seeds.reduce((prev, curr) => {
    const location = locationForSeed(curr);
    return location < prev ? location : prev;
  }, Infinity);

const part2 = () => {};

const ifYouGiveASeedAFertilizer = () =>
  logChallenge({
    name: "Day 5: If You Give A Seed A Fertilizer",
    part1: { run: part1, expected: 579439039 },
    part2: { run: part2, expected: undefined },
  });

ifYouGiveASeedAFertilizer();
