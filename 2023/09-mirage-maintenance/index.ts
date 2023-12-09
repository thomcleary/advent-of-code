import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

type History = {
  values: number[];
  next: History | null;
  prediction: number;
};

const newHistory = (values: History["values"]): History => ({
  values,
  next: null,
  prediction: Infinity,
});

const predictHistory = (history: History) => {
  history.next = getNextHistory(history);
  history.prediction = history.values[history.values.length - 1]! + history.next!.prediction!;
  return history;
};

const getNextHistory = (previousHistory: History): History => {
  const history = newHistory(
    previousHistory.values.reduce<number[]>((prev, curr, index, array) => {
      const next = array[index + 1];
      return next !== undefined ? [...prev, next - curr] : prev;
    }, []),
  );

  if (history.values.every((v) => v === 0)) {
    history.prediction = 0;
  } else {
    predictHistory(history);
  }

  return history;
};

const histories = toLines(getPuzzleInput(import.meta.url, { useExample: false })).map((line) =>
  predictHistory(newHistory(line.split(" ").map((num) => Number.parseInt(num)))),
);

const part1 = () => histories.reduce((prev, curr) => prev + curr.prediction, 0);

const part2 = () => {};

const mirageMaintenance = () =>
  logChallenge({
    name: "Day 9: Mirage Maintenance",
    part1: { run: part1, expected: 1955513104 },
    part2: { run: part2, expected: undefined },
  });

mirageMaintenance();
