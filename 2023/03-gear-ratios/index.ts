import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

type Location = [col: number, row: number];

const engineSchematicLines = toLines(getPuzzleInput(import.meta.url));
const colMax = engineSchematicLines[0]?.length! - 1;
const rowMax = engineSchematicLines.length - 1;

const gear = "*";

const digitsRegex = /((?<left>\d+)[\D])|([\D](?<middle>\d+)[\D])|([\D](?<right>\d+))/g;

const getDigits = ({ groups, index }: RegExpExecArray) => ({
  digits: (groups?.left ?? groups?.middle ?? groups?.right)!,
  col: !!groups?.left ? index : index + 1,
});

const getLocationsToCheck = ({ digits, col, row }: { digits: string; col: number; row: number }) => {
  const colsToCheck = Array.from({ length: digits.length + 2 }, (_, i) => i + (col - 1));

  const top = row === 0 ? [] : (colsToCheck.map((col) => [col, row - 1]) satisfies Location[]);
  const bottom = row === rowMax ? [] : (colsToCheck.map((col) => [col, row + 1]) satisfies Location[]);
  const left = [col - 1, row] satisfies Location;
  const right = [col + digits.length, row] satisfies Location;

  let locations = [...top, ...bottom];
  col !== 0 && locations.push(left);
  col !== colMax && locations.push(right);
  return locations;
};

const part1 = () => {
  let total = 0;
  engineSchematicLines.forEach((line, row) => {
    let execResult: ReturnType<typeof digitsRegex.exec>;
    while ((execResult = digitsRegex.exec(line))) {
      const { digits, col } = getDigits(execResult);
      const isPartNumber = getLocationsToCheck({ digits, col, row }).reduce(
        (prev, [col, row]) => prev || !!engineSchematicLines[row]![col]?.match(/[^\d.]/g),
        false,
      );
      if (isPartNumber) {
        total += Number.parseInt(digits);
      }
    }
  });
  return total;
};

const part2 = () => {
  const gears = new Map<string, number[]>();

  engineSchematicLines.forEach((line, row) => {
    let execResult: ReturnType<typeof digitsRegex.exec>;
    while ((execResult = digitsRegex.exec(line))) {
      const { digits, col } = getDigits(execResult);
      getLocationsToCheck({ digits, row, col }).forEach(([col, row]) => {
        const key = `${row},${col}`;
        engineSchematicLines[row]![col] === gear &&
          gears.set(key, [...(gears.get(key) ?? []), Number.parseInt(digits)]);
      });
    }
  });

  return [...gears.values()]
    .filter((gearParts): gearParts is [number, number] => gearParts.length === 2)
    .map(([first, last]) => first * last)
    .reduce((prev, curr) => prev + curr, 0);
};

const gearRatios = () =>
  logChallenge({
    name: "Day 3: Gear Ratios",
    part1: { run: part1, expected: 507214 },
    part2: { run: part2, expected: 72553319 },
  });

gearRatios();
