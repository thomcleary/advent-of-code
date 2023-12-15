import chalk from "chalk";
import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

type Galaxy = { number: number; row: number; column: number };

const GALAXY = "#";
const EMPTY_SPACE = ".";

const getImage = () => toLines(getPuzzleInput(import.meta.url, { useExample: false })).map((line) => line.split(""));

const logImage = (image: string[][]) => {
  const border = chalk.dim("~").repeat(image[0]!.length + 2);
  console.log(border);
  image.forEach((row) => {
    const colouredRow = row.map((pixel) => (pixel === "." ? chalk.blue(pixel) : chalk.yellow(pixel)));
    process.stdout.write(" " + colouredRow.join("") + "\n");
  });
  console.log(border);
};

const transpose = <T>(array: T[][]) => array[0]!.map((_, colIndex) => array.map((row) => row[colIndex]!));

const getRowsToExpand = (image: string[][]) =>
  image.map((row, index) => (!row.includes(GALAXY) ? index : undefined)).filter((i): i is number => i !== undefined);

const expandImage = (image: string[][]) => {
  getRowsToExpand(image).forEach((rowIndex, index) =>
    image.splice(rowIndex + 1 + index, 0, EMPTY_SPACE.repeat(image[0]!.length).split("")),
  );
  getRowsToExpand(transpose(image)).forEach((columnIndex, index) => {
    image.forEach((row) => row.splice(columnIndex + 1 + index, 0, EMPTY_SPACE));
  });
};

const getGalaxyPairs = (image: string[][]) => {
  const galaxies: Galaxy[] = [];

  let galaxyCount = 1;
  image.forEach((row, rowIndex) => {
    row.forEach((pixel, pixelIndex) => {
      if (pixel === GALAXY) {
        image[rowIndex]![pixelIndex] = galaxyCount.toString();
        galaxies.push({ number: galaxyCount, row: rowIndex, column: pixelIndex });
        galaxyCount++;
      }
    });
  });

  return galaxies.flatMap((from, index) =>
    galaxies.slice(index + 1).map((to) => [from, to] satisfies [Galaxy, Galaxy]),
  );
};

const part1 = () => {
  const image = getImage();
  expandImage(image);
  return getGalaxyPairs(image).reduce(
    (prev, [from, to]) => prev + Math.abs(from.row - to.row) + Math.abs(from.column - to.column),
    0,
  );
};

const part2 = () => {};

const cosmicExpansion = () =>
  logChallenge({
    name: "Day 11: Cosmic Expansion",
    part1: { run: part1, expected: 9648398 },
    part2: { run: part2, expected: undefined },
  });

cosmicExpansion();
