import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

const wordsToDigits: Record<string, number> = {
  one: 1,
  two: 2,
  three: 3,
  four: 4,
  five: 5,
  six: 6,
  seven: 7,
  eight: 8,
  nine: 9,
};
const digitWords = Object.keys(wordsToDigits);

const reverseString = (s: string) => s.split("").reverse().join("");
const sum = (a: number, b: number) => a + b;

const regex = ({ reverseDigitWords }: { reverseDigitWords?: boolean } = {}) => {
  const wordsToMatch = reverseDigitWords ? [...digitWords].map(reverseString) : digitWords;
  return new RegExp(`(${wordsToMatch.join("|")}|[1-9])`);
};

const part1 = (lines: string[]) =>
  lines
    .map((line) => line.replace(/\D/g, ""))
    .map((line) => Number.parseInt(`${line.at(0)}${line.at(line.length - 1)}`))
    .reduce(sum, 0);

const part2 = (lines: string[]) => {
  const forwards = regex();
  const backwards = regex({ reverseDigitWords: true });

  return lines
    .map((line, index) => {
      const first = line.match(forwards)?.[0];
      if (!first) {
        throw new Error(`[Part 2] No match found on line ${index} searching forwards`);
      }
      const last = reverseString(line).match(backwards)?.[0] ?? first;
      return Number.parseInt(
        [first, reverseString(last)].map((digit) => wordsToDigits[digit]?.toString() ?? digit).join(""),
      );
    })
    .reduce(sum, 0);
};

const trebuchet = async () => {
  const calibrationDocumentLines = toLines(await getPuzzleInput(import.meta.url));

  logChallenge({
    name: "Trebuchet?!",
    part1: { run: () => part1(calibrationDocumentLines), expected: 57346 },
    part2: { run: () => part2(calibrationDocumentLines), expected: 57345 },
  });
};

await trebuchet();
