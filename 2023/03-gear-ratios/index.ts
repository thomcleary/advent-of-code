import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

type Location = [col: number, row: number];

const part1 = (engineSchematicLines: string[]) => {
  const isPartNumber = ({ digits, col, row }: { digits: string; col: number; row: number }) => {
    const colMax = engineSchematicLines[0]?.length! - 1;
    const rowMax = engineSchematicLines.length - 1;
    const colsToCheck = Array.from({ length: digits.length + 2 }, (_, i) => i + (col - 1));

    const top = row === 0 ? [] : (colsToCheck.map((col) => [col, row - 1]) satisfies Location[]);
    const bottom = row === rowMax ? [] : (colsToCheck.map((col) => [col, row + 1]) satisfies Location[]);
    const left = [col - 1, row] satisfies Location;
    const right = [col + digits.length, row] satisfies Location;

    let locations = [...top, ...bottom];
    col !== 0 && locations.push(left);
    col !== colMax && locations.push(right);

    return locations.reduce((prev, [col, row]) => prev || !!engineSchematicLines[row]![col]?.match(/[^\d.]/g), false);
  };

  const digitsRegex = /((?<left>\d+)[\D])|([\D](?<middle>\d+)[\D])|([\D](?<right>\d+))/g;
  let total = 0;

  engineSchematicLines.forEach((line, row) => {
    let execResult: ReturnType<typeof digitsRegex.exec>;
    while ((execResult = digitsRegex.exec(line))) {
      const digitGroups = execResult.groups;
      const digits = (digitGroups?.left ?? digitGroups?.middle ?? digitGroups?.right)!;
      const col = !!digitGroups?.left ? execResult.index : execResult.index + 1;

      if (isPartNumber({ digits, col, row })) {
        total += Number.parseInt(digits);
      }
    }
  });

  return total;
};

const gearRatios = async () => {
  const engineSchematicLines = toLines(await getPuzzleInput(import.meta.url));

  logChallenge({
    name: "Day 3: Gear Ratios",
    part1: { run: () => part1(engineSchematicLines), expected: 507214 },
    part2: undefined,
  });
};

await gearRatios();
