import { getPuzzleInput, logChallenge } from "../utils.js";

type NumberRange = [number, number];
type Range = { destStart: number; sourceStart: number; length: number };
type CategoryMap = { to: string; ranges: Range[] };
type AlmanacMaps = Record<string, CategoryMap>;

const almanac = getPuzzleInput(import.meta.url).split("\n\n");

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

const getDestinationRanges = ({ source, numberRange }: { source: string; numberRange: NumberRange }): NumberRange[] => {
  const [min, range] = numberRange;
  const max = min + range - 1;

  let validRanges: NumberRange[] = [];

  for (const sourceRange of maps[source]?.ranges ?? []) {
    const sourceRangeMax = sourceRange.sourceStart + sourceRange.length - 1;
    const destinationRangeMin = sourceRange.destStart + (min - sourceRange.sourceStart);

    const minMatch = min >= sourceRange.sourceStart && min <= sourceRangeMax;
    const maxMatch = max <= sourceRangeMax && max >= sourceRange.sourceStart;

    if (minMatch && maxMatch) {
      validRanges = [[destinationRangeMin, range]];
      break;
    } else if (minMatch) {
      const validRange = sourceRangeMax - min + 1;
      validRanges = [
        [destinationRangeMin, validRange],
        ...getDestinationRanges({ source, numberRange: [sourceRangeMax + 1, range - validRange - 1 - 1] }),
      ];
      break;
    }
  }

  return validRanges;
};

const part1 = () =>
  seeds.reduce((prev, curr) => {
    const location = locationForSeed(curr);
    return location < prev ? location : prev;
  }, Infinity);

const part2 = () =>
  seeds
    .map((seedMin, index) => [seedMin, seeds[index + 1]!] satisfies NumberRange)
    .filter((_, index) => index % 2 === 0)
    .reduce((prev, curr) => {
      let source: string = "seed";
      let numberRanges = [curr];

      while (source !== "location") {
        numberRanges = numberRanges.reduce<NumberRange[]>(
          (prev, curr) => [...prev, ...getDestinationRanges({ source, numberRange: curr })],
          [],
        );
        source = maps[source]?.to ?? "";
      }

      return Math.min(
        numberRanges.reduce((prev, curr) => (curr[0] < prev ? curr[0] : prev), Infinity),
        prev,
      );
    }, Infinity);

const ifYouGiveASeedAFertilizer = () =>
  logChallenge({
    name: "Day 5: If You Give A Seed A Fertilizer",
    part1: { run: part1, expected: 579439039 },
    part2: { run: part2, expected: 7873084 },
  });

ifYouGiveASeedAFertilizer();
