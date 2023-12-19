import chalk, { ChalkInstance } from "chalk";
import { getPuzzleInput, runPuzzle, wait } from "../utils.js";

const useExample = true;

type Tile = "." | "\\" | "/" | "-" | "|";
type Direction = "up" | "down" | "left" | "right";

type Beam = {
  row: number;
  col: number;
  direction: Direction;
};

type TileState = { tile: Tile; visitedBy: Direction[] };

const tiles = {
  ".": chalk.dim,
  "\\": chalk.red,
  "/": chalk.red,
  "-": chalk.yellow,
  "|": chalk.yellow,
} as const satisfies Record<Tile, ChalkInstance>;

const getContraption = () =>
  getPuzzleInput(import.meta.url, { useExample })
    .split("\n")
    .map((row) => (row.split("") as Tile[]).map((tile) => ({ tile, visitedBy: [] }) as TileState));

const logContraption = (
  part: 1 | 2,
  contraption: ReturnType<typeof getContraption>,
  beams: Beam[],
) => {
  const beamLocations = beams.map(({ row, col }) => `${row},${col}`);
  const colouredContraption = contraption.map((row, rowIndex) =>
    row
      .map(({ tile, visitedBy }, colIndex) => {
        const colour = tiles[tile];
        return beamLocations.includes(`${rowIndex},${colIndex}`)
          ? visitedBy.length > 0
            ? chalk.bold.bgGreen(" " + tile + " ")
            : chalk.bold.greenBright(" " + tile + " ")
          : visitedBy.length > 0
            ? colour.bgGreen(" " + tile + " ")
            : colour(" " + tile + " ");
      })
      .join(" "),
  );

  console.log(chalk.dim.underline(`Part ${part}`));
  console.log();
  colouredContraption.forEach((row) => console.log(row + "\n"));
  console.log();
};

const part1 = async () => {
  const contraption = getContraption();

  let beams: Beam[] = [{ row: 0, col: 0, direction: "right" }];

  while (beams.length > 0) {
    if (useExample) {
      console.clear();
      logContraption(1, contraption, beams);
    }

    let nextBeams: Beam[] = [];
    beams.forEach((b) => {
      const row = contraption[b.row];
      const tile = row?.[b.col];

      if (!row || !tile) {
        return;
      } else {
        row[b.col]!.visitedBy.push(b.direction);
      }

      if (tile.tile === ".") {
        switch (b.direction) {
          case "up":
            b.row--;
            break;
          case "down":
            b.row++;
            break;
          case "left":
            b.col--;
            break;
          case "right":
            b.col++;
            break;
        }
      } else if (tile.tile === "|") {
        if (b.direction === "up") {
          b.row--;
        } else {
          b.row++;

          if (b.direction === "left" || b.direction === "right") {
            nextBeams.push({
              row: b.row - 2,
              col: b.col,
              direction: "up",
            });
          }

          b.direction = "down";
        }
      } else if (tile.tile === "-") {
        if (b.direction === "right") {
          b.col++;
        } else {
          b.col--;

          if (b.direction === "up" || b.direction === "down") {
            nextBeams.push({
              row: b.row,
              col: b.col + 2,
              direction: "right",
            });
          }

          b.direction = "left";
        }
      } else if (tile.tile === "/") {
        switch (b.direction) {
          case "up":
            b.col++;
            b.direction = "right";
            break;
          case "down":
            b.col--;
            b.direction = "left";
            break;
          case "left":
            b.row++;
            b.direction = "down";
            break;
          case "right":
            b.row--;
            b.direction = "up";
        }
      } else {
        switch (b.direction) {
          case "up":
            b.col--;
            b.direction = "left";
            break;
          case "down":
            b.col++;
            b.direction = "right";
            break;
          case "left":
            b.row--;
            b.direction = "up";
            break;
          case "right":
            b.row++;
            b.direction = "down";
        }
      }

      nextBeams.push(b);
    });

    beams = nextBeams.filter((b) => {
      const row = contraption[b.row];
      const tile = row?.[b.col];
      const cycle = row && tile && tile.visitedBy.includes(b.direction);
      return !!row && !!tile && !cycle;
    });

    useExample && (await wait(33));
  }

  if (useExample) {
    console.clear();
    logContraption(1, contraption, beams);
  }

  return contraption.reduce(
    (total, currRow) =>
      total + currRow.reduce((prev, curr) => prev + (curr.visitedBy.length > 0 ? 1 : 0), 0),
    0,
  );
};

const part2 = async () => {};

/**
 * @description https://adventofcode.com/2023/day/16
 */
runPuzzle({
  day: 16,
  name: "The Floor Will Be Lava",
  part1: { run: part1, expected: useExample ? 46 : 7884 },
  part2: { run: part2, expected: useExample ? undefined : undefined },
});
