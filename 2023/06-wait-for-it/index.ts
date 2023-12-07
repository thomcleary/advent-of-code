import { getPuzzleInput, logChallenge } from "../utils.js";

type Race = [time: number, distance: number];

const timesAndDistances = (await getPuzzleInput(import.meta.url)).split("\n").map(
  (line) =>
    line
      .split(":")[1]
      ?.trim()
      .replace(/\s+/g, " ")
      .split(" ")
      .map((number) => Number.parseInt(number)),
) as [times: number[], distances: number[]];

const races = timesAndDistances[0].map((time, index) => [time, timesAndDistances[1][index]!]) satisfies Race[];

const part1 = () =>
  races.reduce((prev, [raceTime, requiredDistance]) => {
    let wins = 0;
    for (let i = 0; i <= raceTime; i++) {
      const speed = i;
      const distance = speed * (raceTime - i);
      wins = distance > requiredDistance ? wins + 1 : wins;
    }
    return prev * wins;
  }, 1);

const part2 = () => {};

const waitForIt = () =>
  logChallenge({
    name: "Day 6: Wait For It",
    part1: { run: part1, expected: 220320 },
    part2: { run: part2, expected: undefined },
  });

waitForIt();
