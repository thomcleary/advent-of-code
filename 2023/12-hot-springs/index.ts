import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

type ConditionRecord = { record: string; groups: number[] };

const OPERATIONAL = ".";
const DAMAGED = "#";

const useExample = false;

const conditionRecords = (): ConditionRecord[] => {
  return toLines(getPuzzleInput(import.meta.url, { useExample }))
    .map((line) => line.split(" ") as [string, string])
    .map((line) => {
      const record = line[0];
      const groups = line[1].split(",").map((group) => Number.parseInt(group));

      return { record, groups };
    });
};

const numberOfCombinations = ({ record, groups }: ConditionRecord, counting: boolean = false): number => {
  const groupsCopy = [...groups];
  const currentSpring = record.charAt(0);
  const nextSpring = record.charAt(1);

  if (record === "") {
    return counting || groups.length > 0 ? 0 : 1;
  }

  if (currentSpring === OPERATIONAL) {
    if (counting) {
      return 0;
    }

    return numberOfCombinations({ record: record.slice(1), groups }, false);
  }

  if (currentSpring === DAMAGED) {
    --groupsCopy[0];

    if (groupsCopy[0] === 0) {
      if (nextSpring.startsWith(DAMAGED)) {
        return 0;
      }
      groupsCopy.shift();
      return numberOfCombinations({ record: record.slice(2), groups: groupsCopy }, false);
    }

    return numberOfCombinations({ record: record.slice(1), groups: groupsCopy }, true);
  }

  const whenOperationalNext = counting
    ? 0
    : numberOfCombinations({ record: record.slice(1), groups: [...groupsCopy] }, false);

  if (groupsCopy[0] === 1) {
    if (nextSpring.startsWith(DAMAGED)) {
      return whenOperationalNext + 0;
    }

    groupsCopy.shift();
    return whenOperationalNext + numberOfCombinations({ record: record.slice(2), groups: groupsCopy }, false);
  }

  --groupsCopy[0];
  return whenOperationalNext + numberOfCombinations({ record: record.slice(1), groups: groupsCopy }, true);
};

const part1 = () => conditionRecords().reduce((prev, curr) => prev + numberOfCombinations(curr), 0);

const part2 = () => {};

const hotSprings = () =>
  logChallenge({
    name: "Day 12: Hot Springs",
    part1: { run: part1, expected: useExample ? 21 : 6488 },
    part2: { run: part2, expected: undefined },
  });

hotSprings();
