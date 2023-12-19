import { getPuzzleInput, runPuzzle } from "../utils.js";

const useExample = false;

type Tile = "." | "\\" | "/" | "-" | "|";
type Direction = "^" | "v" | "<" | ">";

type Beam = {
  row: number;
  col: number;
  direction: Direction;
};

type TileState = { type: Tile; visitedBy: Direction[] };

const getContraption = () =>
  getPuzzleInput(import.meta.url, { useExample })
    .split("\n")
    .map((row) =>
      (row.split("") as Tile[]).map((tile) => ({ type: tile, visitedBy: [] }) as TileState),
    );

const emptySpace = ({ row, col, direction }: Beam) => {
  switch (direction) {
    case "^":
      return [{ row: row - 1, col, direction }];
    case "v":
      return [{ row: row + 1, col, direction }];
    case "<":
      return [{ row, col: col - 1, direction }];
    case ">":
      return [{ row, col: col + 1, direction }];
  }
};

const verticalSplitter = ({ row, col, direction }: Beam): Beam[] => {
  const newBeam: Beam = {
    row: row + (direction === "^" ? -1 : 1),
    col,
    direction: direction === "^" ? direction : "v",
  };

  return direction === "^" || direction === "v"
    ? [newBeam]
    : [newBeam, { row: row - 1, col, direction: "^" }];
};

const horizontalSplitter = ({ row, col, direction }: Beam): Beam[] => {
  const newBeam: Beam = {
    row,
    col: col + (direction === ">" ? 1 : -1),
    direction: direction === ">" ? direction : "<",
  };

  return direction === "<" || direction === ">"
    ? [newBeam]
    : [newBeam, { row, col: col + 1, direction: ">" }];
};

const mirror = ({
  mirror,
  beam: { row, col, direction },
}: {
  mirror: Extract<Tile, "/" | "\\">;
  beam: Beam;
}): Beam[] => {
  switch (`${mirror}${direction}` as const) {
    case "/^":
    case "\\v":
      return [{ row, col: col + 1, direction: ">" }];
    case "/v":
    case "\\^":
      return [{ row, col: col - 1, direction: "<" }];
    case "/>":
    case "\\<":
      return [{ row: row - 1, col, direction: "^" }];
    case "/<":
    case "\\>":
      return [{ row: row + 1, col, direction: "v" }];
  }
};

const nextBeams = ({ tile, beam }: { tile: Tile; beam: Beam }): Beam[] => {
  switch (tile) {
    case ".":
      return emptySpace(beam);
    case "|":
      return verticalSplitter(beam);
    case "-":
      return horizontalSplitter(beam);
    case "/":
    case "\\":
      return mirror({ mirror: tile, beam });
  }
};

const numTilesEnergized = (initialBeam: Beam) => {
  const contraption = getContraption();

  let beams: Beam[] = [initialBeam];

  while (beams.length > 0) {
    let next: Beam[] = [];

    beams.forEach((beam) => {
      const row = contraption[beam.row];
      const tile = row?.[beam.col];

      if (!tile || tile.visitedBy.includes(beam.direction)) {
        return;
      }

      tile.visitedBy.push(beam.direction);
      next.push(...nextBeams({ tile: tile.type, beam }));
    });

    beams = next;
  }

  return contraption.reduce(
    (total, currRow) =>
      total + currRow.reduce((prev, curr) => prev + (curr.visitedBy.length > 0 ? 1 : 0), 0),
    0,
  );
};

const part1 = () => numTilesEnergized({ row: 0, col: 0, direction: ">" });

const part2 = () => {
  const contraption = getContraption();

  const horizontalStarts = (direction: Extract<Direction, "<" | ">">) =>
    Array(contraption.length)
      .fill(0)
      .map(
        (item, index) =>
          ({
            row: item + index,
            col: direction === ">" ? 0 : contraption.length - 1,
            direction,
          }) satisfies Beam,
      );

  const verticalStarts = (direction: Extract<Direction, "^" | "v">) =>
    Array(contraption[0]?.length)
      .fill(0)
      .map(
        (item, index) =>
          ({
            row: direction === "v" ? 0 : contraption.length - 1,
            col: item + index,
            direction,
          }) satisfies Beam,
      );

  return Math.max(
    ...[
      ...horizontalStarts("<"),
      ...verticalStarts("^"),
      ...horizontalStarts(">"),
      ...verticalStarts("v"),
    ].map(numTilesEnergized),
  );
};

/**
 * @description https://adventofcode.com/2023/day/16
 */
runPuzzle({
  day: 16,
  name: "The Floor Will Be Lava",
  part1: { run: part1, expected: useExample ? 46 : 7884 },
  part2: { run: part2, expected: useExample ? 51 : 8185 },
});
