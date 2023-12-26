import { getPuzzleInput, runPuzzle } from "../utils.js";

const useExample = true;

type CityBlock = {
  row: number;
  column: number;
  direction: "left" | "right" | "up" | "down";
  directionCount: number;
  previousBlock: CityBlock | undefined;
};

class PriorityQueue<T> {
  private queue: { item: T; priority: number }[] = [];

  /**
   * TODO: use a min-heap instead (array + sort is 🐌)
   */
  enqueue(item: T, priority: number) {
    this.queue.push({ item, priority });
    this.queue.sort((a, b) => a.priority - b.priority);
    return this;
  }

  dequeue() {
    return this.queue.shift();
  }

  log() {
    this.queue.forEach(({ item, priority }) => {
      console.log({ item, priority });
    });
  }
}

const cityMap = (() => {
  const map = getPuzzleInput(import.meta.url, { useExample })
    .split("\n")
    .map((row) => row.split("").map((block) => Number.parseInt(block)));
  map[0]![0] = 0;
  return map;
})();

const part1 = () => {
  const pq = new PriorityQueue<CityBlock>();
};

const part2 = () => {};

/**
 * @description https://adventofcode.com/2023/day/17
 */
runPuzzle({
  day: 17,
  name: "Clumsy Crucible",
  part1: { run: part1, expected: useExample ? 102 : undefined },
  part2: { run: part2, expected: useExample ? undefined : undefined },
});
