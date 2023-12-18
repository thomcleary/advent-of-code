import { getPuzzleInput, runPuzzle, toLines } from "../utils.js";

type Scratchcard = [winning: string[], scratched: string[]];

const cards = toLines(getPuzzleInput(import.meta.url))
  .map((line) => line.split("|"))
  .map((card) => [
    card[0]!.split(":")[1]?.trim().replace("  ", " ").split(" "),
    card[1]!.trim().replaceAll("  ", " ").split(" "),
  ]) as Scratchcard[];

const numberOfWinsForCard = ([winning, scratched]: Scratchcard) =>
  scratched.reduce((prev, curr) => (winning.includes(curr) ? prev + 1 : prev), 0);

const part1 = () =>
  cards.reduce((prev, curr) => {
    const wins = numberOfWinsForCard(curr);
    return wins === 0 ? prev : prev + 2 ** (wins - 1);
  }, 0);

const part2 = () =>
  cards
    .map((card) => ({ count: 1, wins: numberOfWinsForCard(card) }))
    .reduce((prev, { count, wins }, currIndex, cards) => {
      for (let j = currIndex + 1; j <= currIndex + wins; j++) {
        cards[j]!.count += count;
      }
      return prev + count;
    }, 0);

/**
 * @description https://adventofcode.com/2023/day/4
 */
runPuzzle({
  day: 4,
  name: "Scratchcards",
  part1: { run: part1, expected: 18519 },
  part2: { run: part2, expected: 11787590 },
});
