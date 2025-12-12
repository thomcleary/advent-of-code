import assert from "node:assert";
import { stdin } from "../utils.ts";

type Corner = { row: number; column: number };

const getCorners = (input: string) =>
  input.split(/\r?\n/).map((line) => {
    const [column, row] = line.split(",").map((n) => parseInt(n));
    assert(row !== undefined && !isNaN(row));
    assert(column !== undefined && !isNaN(column));
    return { row, column } satisfies Corner;
  });

const largestRectangleArea = (corners: Corner[]): number => {
  let largestArea = 0;

  for (let i = 0; i < corners.length - 1; i++) {
    const start = corners.at(i);
    assert(start !== undefined);

    for (let j = i + 1; j < corners.length; j++) {
      const end = corners.at(j);
      assert(end !== undefined);

      const area =
        Math.max(Math.abs(start.row - end.row + 1), 1) *
        Math.max(Math.abs(start.column - end.column + 1), 1);

      largestArea = Math.max(area, largestArea);
    }
  }

  assert(largestArea > 0);

  return largestArea;
};

const main = async () => {
  const corners = getCorners(await stdin());

  console.log(`Part 1: ${largestRectangleArea(corners)}`);
  console.log(`Part 2: TODO`);
};

await main();
