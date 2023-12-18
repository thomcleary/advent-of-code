import { getPuzzleInput, runPuzzle } from "../utils.js";

type Node = { id: string; left: string; right: string };

const { instructions, nodes } = (() => {
  const [instructions, network] = getPuzzleInput(import.meta.url).split("\n\n") as [string, string];
  const nodes = network
    .split("\n")
    .map((node) => node.split(" = "))
    .map((node) => {
      const id = node[0]!;
      const [left, right] = node[1]?.replaceAll(/[\(\)]/g, "").split(", ") as [string, string];
      return { id, left, right };
    })
    .reduce<Record<string, Node>>((prev, curr) => ({ ...prev, [curr.id!]: curr }), {});

  return { instructions: instructions.trim().split(""), nodes };
})();

const steps = ({ from, to }: { from: string; to: RegExp }) => {
  let steps = 0;
  let currentNode = nodes[from]!;

  while (!currentNode.id.match(to)) {
    currentNode =
      nodes[
        instructions[steps % instructions.length] === "L" ? currentNode.left : currentNode.right
      ]!;
    steps++;
  }

  return steps;
};

const gcd = (m: number, n: number): number => (n === 0 ? m : gcd(n, m % n));

const lcm = (a: number, b: number) => (a * b) / gcd(a, b);

const part1 = () => steps({ from: "AAA", to: /ZZZ/ });

const part2 = () =>
  Object.keys(nodes)
    .filter((key) => key.endsWith("A"))
    .map((key) => steps({ from: nodes[key]!.id, to: /[A-Z]{2}Z/ }))
    .reduce((prev, curr) => lcm(prev, curr));

/**
 * @description https://adventofcode.com/2023/day/8
 */
runPuzzle({
  day: 8,
  name: "Haunted Wasteland",
  part1: { run: part1, expected: 24253 },
  part2: { run: part2, expected: 12357789728873 },
});
