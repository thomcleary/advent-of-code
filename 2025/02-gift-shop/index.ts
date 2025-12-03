import assert from "assert";
import { getStdin } from "../utils.ts";

type Range = {
  start: number;
  end: number;
};

const invalidIdSum = (ranges: Range[], { maxRepeats }: { maxRepeats?: number } = {}): number => {
  return ranges
    .flatMap((range) => {
      let invalid: number[] = [];

      for (let id = range.start; id <= range.end; id++) {
        const digits = id.toString();

        for (let repeats = 2; repeats <= (maxRepeats ?? digits.length); repeats++) {
          if (digits.length % repeats !== 0) {
            continue; // could increment i until there is another digit
          }

          const chunkSize = digits.length / repeats;
          const chunks = new Set<string>();

          for (let i = 0; i < digits.length; i += chunkSize) {
            chunks.add(digits.slice(i, i + chunkSize));
          }

          if (chunks.size === 1) {
            invalid.push(id);
            break;
          }
        }
      }

      return invalid;
    })
    .reduce((a, b) => a + b, 0);
};

const main = async () => {
  const stdin = await getStdin();

  const ranges = stdin
    .trim()
    .split(",")
    .map((r) => {
      const [start, end] = r.split("-");
      assert(start && end);
      return { start: parseInt(start), end: parseInt(end) } satisfies Range;
    });

  console.log(`Part 1: ${invalidIdSum(ranges, { maxRepeats: 2 })}`);
  console.log(`Part 2: ${invalidIdSum(ranges)}`);
};

await main();
