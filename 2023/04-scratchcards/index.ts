import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

const toCards = (puzzleLines: string[]) => {
  return puzzleLines
    .map((line) => line.split("|"))
    .map((card) => [
      new Set(
        card[0]!
          .split(":")[1]
          ?.trim()
          .replace("  ", " ")
          .split(" ")
          .map((num) => Number.parseInt(num)),
      ),
      new Set(
        card[1]!
          .trim()
          .replaceAll("  ", " ")
          .split(" ")
          .map((num) => Number.parseInt(num)),
      ),
    ]) as [winning: Set<number>, scratched: Set<number>][];
};

const cards = toCards(toLines(await getPuzzleInput(import.meta.url)));

const part1 = () =>
  cards.reduce((prev, [winning, scratched]) => {
    const winningScratched = [...scratched.values()].reduce((prev, curr) => (winning.has(curr) ? prev + 1 : prev), 0);
    return winningScratched === 0 ? prev : prev + 2 ** (winningScratched - 1);
  }, 0);

const part2 = () => {};

const scratchcards = () =>
  logChallenge({
    name: "Day 4 Scratchcards",
    part1: { run: part1, expected: 18519 },
    part2: { run: part2, expected: undefined },
  });

scratchcards();
