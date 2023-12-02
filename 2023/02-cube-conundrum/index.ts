import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

const part1 = (games: string[]) => {
  const bag = { red: 12, green: 13, blue: 14 } as const;

  return games
    .map((game) => game.split(":"))
    .map((g) => ({
      id: Number.parseInt(g[0]?.replaceAll(/\D/g, "") ?? "Invalid Game ID"),
      valid: g[1]
        ?.split(";")
        .map((set) =>
          set
            .split(",")
            .map((cubeCount) => cubeCount.trim().split(" ") as [string, keyof typeof bag])
            .reduce((prev, curr) => prev && Number.parseInt(curr[0]) <= bag[curr[1]], true),
        )
        .reduce((prev, curr) => prev && curr, true),
    }))
    .reduce((prev, curr) => (curr.valid ? curr.id + prev : prev), 0);
};

const cubeConundrum = async () => {
  const games = toLines(await getPuzzleInput(import.meta.url));

  logChallenge({
    name: "Day 2: Cube Conundrum",
    part1: { run: () => part1(games), expected: 2685 },
    part2: undefined,
  });
};

await cubeConundrum();
