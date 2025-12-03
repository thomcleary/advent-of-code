import assert from "assert";
import { getStdin } from "../utils.ts";

type Range = {
  start: number;
  end: number;
};

const toRanges = (stdin: string) => {
  return stdin
    .trim()
    .split(",")
    .map((r) => {
      const [start, end] = r.split("-");
      assert(start && end);
      return { start: parseInt(start), end: parseInt(end) } satisfies Range;
    });
};

const findInvalidIds = (ranges: Range[]): number[] => {
  return ranges.flatMap((range) => {
    let invalid: number[] = [];

    for (let i = range.start; i <= range.end; i++) {
      const digits = i.toString();

      if (digits.length % 2 !== 0) {
        continue; // could increment i until there is another digit
      }

      const middle = digits.length / 2;
      const left = digits.slice(0, middle);
      const right = digits.slice(middle);

      if (left === right) {
        invalid.push(i);
      }
    }

    return invalid;
  });
};

const main = async () => {
  const stdin = await getStdin();

  const ranges = toRanges(stdin);

  const invalidIds = findInvalidIds(ranges);
  const invalidIdSum = invalidIds.reduce((prev, curr) => prev + curr, 0);

  console.log(`Part 1: ${invalidIdSum}`);
  console.log(`Part 2: TODO`);
};

await main();
