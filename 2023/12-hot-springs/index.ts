import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

type ConditionRecord = { record: string; groups: number[] };

const useExample = false;

const OPERATIONAL = ".";
const DAMAGED = "#";

const cache = new Map<string, number>();

const conditionRecords = (): ConditionRecord[] => {
  return toLines(getPuzzleInput(import.meta.url, { useExample }))
    .map((line) => line.split(" ") as [string, string])
    .map((line) => {
      const record = line[0];
      const groups = line[1].split(",").map((group) => Number.parseInt(group));

      return { record, groups };
    });
};

const unfoldRecords = (records: ConditionRecord[]): ConditionRecord[] =>
  records.map(({ record, groups }) => ({
    record: Array(5).fill(record).join("?"),
    groups: Array(5)
      .fill(groups)
      .flatMap((g) => g),
  }));

const numberOfCombinations = ({ record, groups }: ConditionRecord, counting: boolean = false): number => {
  const cacheKey = `record:${record};groups:${groups.join(",")};counting:${counting.toString()}`;

  if (cache.has(cacheKey)) {
    return cache.get(cacheKey)!;
  }

  const groupsCopy = [...groups];
  const currentSpring = record.charAt(0);
  const nextSpringIsDamaged = record.charAt(1).startsWith(DAMAGED);

  let combinations: number;

  if (record === "") {
    combinations = groups.length > 0 ? 0 : 1;
  } else if (currentSpring === OPERATIONAL) {
    combinations = counting ? 0 : numberOfCombinations({ record: record.slice(1), groups }, false);
  } else if (currentSpring === DAMAGED) {
    if (groupsCopy[0] === 1) {
      combinations = nextSpringIsDamaged
        ? 0
        : numberOfCombinations({ record: record.slice(2), groups: groupsCopy.slice(1) }, false);
    } else {
      groupsCopy[0]--;
      combinations = numberOfCombinations({ record: record.slice(1), groups: groupsCopy }, true);
    }
  } else {
    const operationalNext = counting ? 0 : numberOfCombinations({ record: record.slice(1), groups: groupsCopy }, false);

    if (groupsCopy[0] === 1) {
      combinations =
        operationalNext +
        (nextSpringIsDamaged
          ? 0
          : numberOfCombinations({ record: record.slice(2), groups: groupsCopy.slice(1) }, false));
    } else {
      groupsCopy[0]--;
      combinations = operationalNext + numberOfCombinations({ record: record.slice(1), groups: groupsCopy }, true);
    }
  }

  cache.set(cacheKey, combinations);
  return combinations;
};

const combinationSum = (records: ConditionRecord[]) =>
  records.reduce((prev, curr) => prev + numberOfCombinations(curr), 0);

const part1 = () => combinationSum(conditionRecords());

const part2 = () => combinationSum(unfoldRecords(conditionRecords()));

const hotSprings = () =>
  logChallenge({
    name: "Day 12: Hot Springs",
    part1: { run: part1, expected: useExample ? 21 : 6488 },
    part2: { run: part2, expected: useExample ? 525152 : 815364548481 },
  });

hotSprings();
