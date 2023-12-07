import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

type Race = [time: number, distance: number];

const getTimesAndDistances = () =>
  toLines(getPuzzleInput(import.meta.url)).map(
    (line) =>
      line
        .split(":")[1]
        ?.trim()
        .replace(/\s+/g, " ")
        .split(" ")
        .map((number) => Number.parseInt(number)),
  ) as [times: number[], distances: number[]];

const getRaces = () => {
  const timesAndDistances = getTimesAndDistances();
  return timesAndDistances[0].map((time, index) => [time, timesAndDistances[1][index]!]) satisfies Race[];
};

const fixKerning = (races: ReturnType<typeof getTimesAndDistances>) =>
  [races[0]!.join(""), races[1]!.join("")].map((number) => Number.parseInt(number)) as Race;

const getWins = ([raceTime, requiredDistance]: Race) => {
  let wins = 0;
  for (let speed = 0; speed <= raceTime; speed++) {
    const distance = speed * (raceTime - speed);
    wins += distance > requiredDistance ? 1 : 0;
  }
  return wins;
};

const part1 = () => getRaces().reduce((prev, curr) => prev * getWins(curr), 1);

const part2 = () => getWins(fixKerning(getTimesAndDistances()));

const waitForIt = () =>
  logChallenge({
    name: "Day 6: Wait For It",
    part1: { run: part1, expected: 220320 },
    part2: { run: part2, expected: 34454850 },
  });

waitForIt();
