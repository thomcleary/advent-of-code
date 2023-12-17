import { getPuzzleInput, logChallenge, toLines, transpose } from "../utils.js";

type Galaxy = { number: number; row: number; column: number };

const GALAXY = "#";

const useExample = false;

const getImage = () => toLines(getPuzzleInput(import.meta.url, { useExample })).map((line) => line.split(""));

const getRowsToExpand = (image: string[][]) =>
  image.map((row, index) => (!row.includes(GALAXY) ? index : undefined)).filter((i): i is number => i !== undefined);

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

const galaxyPairsDistanceSum = (expansion: number) => {
  const image = getImage();
  const rowsToExpand = getRowsToExpand(image);
  const columnsToExpand = getRowsToExpand(transpose(image));

  return getGalaxyPairs(image).reduce((prev, [from, to]) => {
    let distance = Math.abs(from.row - to.row) + Math.abs(from.column - to.column);

    rowsToExpand.forEach((row) => {
      if (row > from.row && row < to.row) {
        distance += expansion;
      }
    });

    columnsToExpand.forEach((column) => {
      const betweenLeftToRight = from.column < to.column && column > from.column && column < to.column;
      const betweenRightToLeft = from.column > to.column && column < from.column && column > to.column;
      if (betweenLeftToRight || betweenRightToLeft) {
        distance += expansion;
      }
    });

    return prev + distance;
  }, 0);
};

const part1 = () => galaxyPairsDistanceSum(1);

const part2 = () => galaxyPairsDistanceSum(useExample ? 99 : 999_999);

const cosmicExpansion = () =>
  logChallenge({
    name: "Day 11: Cosmic Expansion",
    part1: { run: part1, expected: useExample ? 374 : 9648398 },
    part2: { run: part2, expected: useExample ? 8410 : 618800410814 },
  });

cosmicExpansion();
