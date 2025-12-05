import assert from "assert";
import { stdin } from "../utils.ts";

type Position = { row: number; col: number };
type PositionKey = `${number},${number}`;

const adjacencies = (position: Position) => [
  { row: position.row - 1, col: position.col - 1 },
  { row: position.row - 1, col: position.col },
  { row: position.row - 1, col: position.col + 1 },
  { row: position.row, col: position.col - 1 },
  { row: position.row, col: position.col + 1 },
  { row: position.row + 1, col: position.col - 1 },
  { row: position.row + 1, col: position.col },
  { row: position.row + 1, col: position.col + 1 },
];

const positionToKey = (position: Position): PositionKey => `${position.row},${position.col}`;
const keyToPosition = (key: PositionKey): Position => {
  const [row, col] = key.split(",").map((v) => parseInt(v));
  assert(row !== undefined && col !== undefined);
  return { row, col };
};

const adjacentCounts = ({ diagram }: { diagram: string[][] }): Map<PositionKey, number> => {
  const adjacentCounts = new Map<PositionKey, number>();

  diagram.forEach((row, r) => {
    row.forEach((tile, c) => {
      if (tile !== "@") {
        return;
      }

      const rollPosition = { row: r, col: c };
      const rollKey = positionToKey(rollPosition);
      adjacentCounts.set(rollKey, 0);

      adjacencies(rollPosition).forEach((adjacentPosition) => {
        if (diagram[adjacentPosition.row]?.[adjacentPosition.col] === "@") {
          const count = adjacentCounts.get(rollKey);
          assert(count !== undefined);
          adjacentCounts.set(rollKey, count + 1);
        }
      });
    });
  });

  return adjacentCounts;
};

const rollsAccessibleByForklift = (adjacencies: Map<PositionKey, number>) =>
  [...adjacencies.entries()].filter(([pos, count]) => count < 4);

const main = async () => {
  const diagram = (await stdin()).split("\n").map((line) => line.split(""));

  const adjacencyCounts = adjacentCounts({ diagram });

  let accessibleRolls = rollsAccessibleByForklift(adjacencyCounts);
  const rollsInitiallyAccessible = accessibleRolls.length;
  let rollsRemoved = 0;

  while (accessibleRolls.length > 0) {
    accessibleRolls.forEach(([rollKey]) => {
      const rollPosition = keyToPosition(rollKey);

      adjacencies(rollPosition).forEach((adjacentPosition) => {
        const adjacentKey = positionToKey(adjacentPosition);
        const adjacentCount = adjacencyCounts.get(adjacentKey);

        if (adjacentCount) {
          adjacencyCounts.set(adjacentKey, adjacentCount - 1);
        }
      });

      adjacencyCounts.delete(rollKey);
    });

    rollsRemoved += accessibleRolls.length;
    accessibleRolls = rollsAccessibleByForklift(adjacencyCounts);
  }

  console.log(`Part 1: ${rollsInitiallyAccessible}`);
  console.log(`Part 2: ${rollsRemoved}`);
};

await main();
