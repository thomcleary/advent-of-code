import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

const calibrationDocumentLines = toLines(getPuzzleInput(import.meta.url));

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
const forwards = regex();
const backwards = regex({ reverseDigitWords: true });

const part1 = () =>
  calibrationDocumentLines
    .map((line) => line.replace(/\D/g, ""))
    .map((line) => Number.parseInt(`${line.at(0)}${line.at(line.length - 1)}`))
    .reduce(sum, 0);

const part2 = () =>
  calibrationDocumentLines
    .map((line) => {
      const first = line.match(forwards)![0];
      const last = reverseString(line).match(backwards)?.[0] ?? first;
      return Number.parseInt(
        [first, reverseString(last)].map((digit) => wordsToDigits[digit]?.toString() ?? digit).join(""),
      );
    })
    .reduce(sum, 0);

const trebuchet = () =>
  logChallenge({
    name: "Day 1: Trebuchet?!",
    part1: { run: part1, expected: 57346 },
    part2: { run: part2, expected: 57345 },
  });

trebuchet();
