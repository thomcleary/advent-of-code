import { getPuzzleInput, runPuzzle, toLines } from "../utils.js";

type Tile = "|" | "-" | "L" | "J" | "7" | "F" | "." | "S";
type Direction = "Up" | "Down" | "Left" | "Right" | "Stationary";
type Point = [row: number, col: number];

const useExample = false;

const { maze, startPoint } = (() => {
  const maze = toLines(getPuzzleInput(import.meta.url, { useExample })).map((line) =>
    line.split(""),
  ) as Tile[][];
  const startRow = maze
    .map((row, rowNum) => ({ row, rowNum }))
    .find((row) => row.row.includes("S"))!;
  return { maze, startPoint: [startRow.rowNum, startRow.row.indexOf("S")!] as Point };
})();

const getDirection = ({
  currentPoint: [currentRow, currentColumn],
  previousPoint: [previousRow, previousColumn],
}: {
  currentPoint: Point;
  previousPoint: Point;
}) => {
  const verticalDirection: Direction =
    currentRow > previousRow ? "Down" : currentRow < previousRow ? "Up" : "Stationary";
  const horizontalDirection: Direction =
    currentColumn > previousColumn
      ? "Right"
      : currentColumn < previousColumn
        ? "Left"
        : "Stationary";

  return verticalDirection !== "Stationary" ? verticalDirection : horizontalDirection;
};

const getTile = (point: Point) => maze[point[0]]![point[1]]!;

const up = (point: Point): Point => [point[0] - 1, point[1]];
const down = (point: Point): Point => [point[0] + 1, point[1]];
const left = (point: Point): Point => [point[0], point[1] - 1];
const right = (point: Point): Point => [point[0], point[1] + 1];

const getInitialPoint = (): Point => {
  const u = up(startPoint);
  const d = down(startPoint);
  const l = left(startPoint);
  const r = right(startPoint);

  switch (true) {
    case ["|", "7", "F"].includes(getTile(u)):
      return u;
    case ["|", "L", "J"].includes(getTile(d)):
      return d;
    case ["-", "L", "F"].includes(getTile(l)):
      return l;
    default:
      return r;
  }
};

const getNextPoint = ({
  currentPoint,
  previousPoint,
}: {
  currentPoint: Point;
  previousPoint: Point;
}): Point => {
  const direction = getDirection({ currentPoint, previousPoint });
  const currentTile = maze[currentPoint[0]]![currentPoint[1]];

  switch (currentTile) {
    case "|":
      return direction === "Up" ? up(currentPoint) : down(currentPoint);
    case "-":
      return direction === "Left" ? left(currentPoint) : right(currentPoint);
    case "L":
      return direction === "Left" ? up(currentPoint) : right(currentPoint);
    case "J":
      return direction === "Right" ? up(currentPoint) : left(currentPoint);
    case "7":
      return direction === "Right" ? down(currentPoint) : left(currentPoint);
    case "F":
      return direction === "Left" ? down(currentPoint) : right(currentPoint);
    default:
      throw new Error(
        `[getNextPoint]: Current tile ${currentTile} at point [row: ${currentPoint[0]}, column: ${currentPoint[1]}] is not a valid tile`,
      );
  }
};

const getLoop = () => {
  let previousPoint = startPoint;
  let currentPoint = getInitialPoint();
  let points: Point[] = [currentPoint];

  while (currentPoint[0] !== startPoint[0] || currentPoint[1] !== startPoint[1]) {
    const nextPoint = getNextPoint({ currentPoint, previousPoint });
    previousPoint = currentPoint;
    currentPoint = nextPoint;
    points.push(currentPoint);
  }

  return points;
};

// https://en.wikipedia.org/wiki/Shoelace_formula
const getLoopArea = (loop: Point[]) => {
  let currentPoint = 0;
  let total = 0;

  while (currentPoint < loop.length - 1) {
    total +=
      loop[currentPoint]![0] * loop[currentPoint + 1]![1] -
      loop[currentPoint]![1] * loop[currentPoint + 1]![0];
    currentPoint++;
  }

  total += loop[currentPoint]![0] * loop[0]![1] - loop[currentPoint]![1] * loop[0]![0];

  return total / 2;
};

// https://en.wikipedia.org/wiki/Pick%27s_theorem
const getPointsWithinLoop = (loop: Point[]) => getLoopArea(loop) + 1 - loop.length / 2;

const part1 = () => getLoop().length / 2;

const part2 = () => getPointsWithinLoop(getLoop());

/**
 * @description https://adventofcode.com/2023/day/10
 */
runPuzzle({
  day: 10,
  name: "Pipe Maze",
  part1: { run: part1, expected: useExample ? 23 : 6931 },
  part2: { run: part2, expected: useExample ? 4 : 357 },
});
