import { stdin } from "../utils.ts";

type Position = { row: number; col: number };

const isAccessibleByForklift = ({
  diagram,
  tilePosition,
}: {
  diagram: string[][];
  tilePosition: Position;
}): boolean => {
  const tile = diagram[tilePosition.row]?.[tilePosition.col];

  if (tile !== "@") {
    return false;
  }

  const adjacent = (
    [
      { row: tilePosition.row - 1, col: tilePosition.col - 1 },
      { row: tilePosition.row - 1, col: tilePosition.col },
      { row: tilePosition.row - 1, col: tilePosition.col + 1 },
      { row: tilePosition.row, col: tilePosition.col - 1 },
      { row: tilePosition.row, col: tilePosition.col + 1 },
      { row: tilePosition.row + 1, col: tilePosition.col - 1 },
      { row: tilePosition.row + 1, col: tilePosition.col },
      { row: tilePosition.row + 1, col: tilePosition.col + 1 },
    ] satisfies Position[]
  )
    .map((adj) => diagram[adj.row]?.[adj.col])
    .filter((adj) => adj === "@");

  return adjacent.length < 4;
};

const main = async () => {
  const diagram = (await stdin()).split("\n").map((line) => line.split(""));

  const rollsAccessibleByForklift = diagram
    .flatMap((row, r) => row.map((tile, c) => isAccessibleByForklift({ diagram, tilePosition: { row: r, col: c } })))
    .filter(Boolean).length;

  console.log(`Part 1: ${rollsAccessibleByForklift}`);
  console.log(`Part 2: TODO`);
};

await main();
