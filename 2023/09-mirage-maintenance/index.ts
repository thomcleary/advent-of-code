import { getPuzzleInput, runPuzzle, toLines } from "../utils.js";

type History = {
  values: number[];
  next: History | null;
  prediction: number;
};

type Direction = "forwards" | "backwards";

const newHistory = (values: History["values"]): History => ({
  values,
  next: null,
  prediction: Infinity,
});

const predictHistory = ({ history, direction }: { history: History; direction: Direction }) => {
  history.next = getNextHistory({ previousHistory: history, direction });

  const lastValue = history.values[history.values.length - 1]!;
  const nextPrediction = history.next.prediction;
  history.prediction =
    direction === "forwards" ? lastValue + nextPrediction : lastValue - nextPrediction;

  return history;
};

const getNextHistory = ({
  previousHistory,
  direction,
}: {
  previousHistory: History;
  direction: Direction;
}): History => {
  const history = newHistory(
    previousHistory.values.reduce<number[]>((prev, curr, index, array) => {
      const next = array[index + 1];
      return next !== undefined
        ? [...prev, direction === "forwards" ? next - curr : curr - next]
        : prev;
    }, []),
  );

  if (history.values.every((v) => v === 0)) {
    history.prediction = 0;
  } else {
    predictHistory({ history, direction });
  }

  return history;
};

const historyPredictionSum = ({ direction }: { direction: Direction }) =>
  toLines(getPuzzleInput(import.meta.url, { useExample: false }))
    .map((line) => {
      const values = line.split(" ").map((num) => Number.parseInt(num));
      return predictHistory({
        history: newHistory(direction === "forwards" ? values : values.reverse()),
        direction,
      });
    })
    .reduce((prev, curr) => prev + curr.prediction, 0);

const part1 = () => historyPredictionSum({ direction: "forwards" });

const part2 = () => historyPredictionSum({ direction: "backwards" });

/**
 * @description https://adventofcode.com/2023/day/9
 */
runPuzzle({
  day: 9,
  name: "Mirage Maintenance",
  part1: { run: part1, expected: 1955513104 },
  part2: { run: part2, expected: 1131 },
});
