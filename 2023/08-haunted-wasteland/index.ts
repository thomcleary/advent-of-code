import { getPuzzleInput, logChallenge } from "../utils.js";

const getMap = () => {
  const [instructions, network] = getPuzzleInput(import.meta.url).split("\n\n") as [string, string];
  const nodes = network
    .split("\n")
    .map((node) => node.split(" = "))
    .map((node) => {
      const id = node[0]!;
      const [left, right] = node[1]?.replaceAll(/[\(\)]/g, "").split(", ") as [string, string];
      return { id, left, right };
    })
    .reduce<Record<string, { id: string; left: string; right: string }>>(
      (prev, curr) => ({ ...prev, [curr.id!]: curr }),
      {},
    );

  return { instructions: instructions.trim().split(""), nodes };
};

const part1 = () => {
  const { instructions, nodes } = getMap();

  let steps = 0;
  let currentNode = nodes["AAA"]!;

  while (currentNode.id !== "ZZZ") {
    currentNode = nodes[instructions[steps % instructions.length] === "L" ? currentNode.left : currentNode.right]!;
    steps++;
  }

  return steps;
};

const part2 = () => {};

const hauntedWasteland = () =>
  logChallenge({
    name: "Day 8: Haunted Wasteland",
    part1: { run: part1, expected: 24253 },
    part2: { run: part2, expected: undefined },
  });

hauntedWasteland();
