import assert from "node:assert";
import { stdin } from "../utils.ts";

type Position = { x: number; y: number; z: number };

type JunctionBox = {
  id: number;
  position: Position;
};

const getJunctionBoxes = (input: string) =>
  input.split(/\r?\n/).map((line, i) => {
    const [x, y, z] = line.split(",").map((n) => parseInt(n));
    assert(x !== undefined && !isNaN(x));
    assert(y !== undefined && !isNaN(y));
    assert(z !== undefined && !isNaN(z));
    return {
      id: i,
      position: { x, y, z },
    } satisfies JunctionBox;
  });

const euclideanDistance = (p: Position, q: Position): number =>
  Math.sqrt((p.x - q.x) ** 2 + (p.y - q.y) ** 2 + (p.z - q.z) ** 2);

const getDistances = (
  junctionBoxes: JunctionBox[]
): { from: number; to: number; distance: number }[] => {
  const distances: { from: number; to: number; distance: number }[] = [];

  for (let from = 0; from < junctionBoxes.length - 1; from++) {
    for (let to = from + 1; to < junctionBoxes.length; to++) {
      const fromBox = junctionBoxes.at(from);
      const toBox = junctionBoxes.at(to);

      assert(fromBox !== undefined);
      assert(toBox !== undefined);

      distances.push({
        from: fromBox.id,
        to: toBox.id,
        distance: euclideanDistance(fromBox.position, toBox.position),
      });
    }
  }

  return distances;
};

const connectBoxes = ({
  junctionBoxes,
  limit,
}: {
  junctionBoxes: JunctionBox[];
  limit?: number;
}): {
  circuits: Set<number>[];
  lastConnection: { from: JunctionBox; to: JunctionBox };
} => {
  const distances = getDistances(junctionBoxes)
    .sort((a, b) => a.distance - b.distance)
    .slice(0, limit);

  const circuits: Set<number>[] = junctionBoxes.map((box) => new Set([box.id]));

  let lastConnection: { from: number; to: number } | undefined = undefined;

  for (const { from, to } of distances) {
    const fromCircuit = circuits.find((c) => c.has(from));
    assert(fromCircuit !== undefined);

    if (fromCircuit.has(to)) {
      continue;
    }

    const toCircuit = circuits.find((c) => c.has(to));
    assert(toCircuit !== undefined);

    for (const box of toCircuit) {
      fromCircuit.add(box);
      toCircuit.delete(box);
    }

    lastConnection = { from, to };

    if (fromCircuit.size === junctionBoxes.length) {
      break;
    }
  }

  assert(lastConnection !== undefined);

  const lastFromBox = junctionBoxes.find((b) => b.id === lastConnection.from);
  const lastToBox = junctionBoxes.find((b) => b.id === lastConnection.to);

  assert(lastFromBox !== undefined);
  assert(lastToBox !== undefined);

  return {
    circuits: circuits.filter((c) => c.size > 0),
    lastConnection: { from: lastFromBox, to: lastToBox },
  };
};

const main = async () => {
  const junctionBoxes = getJunctionBoxes(await stdin());

  const part1 = connectBoxes({ junctionBoxes, limit: 1000 })
    .circuits.map((c) => c.size)
    .sort((a, b) => b - a)
    .slice(0, 3)
    .reduce((a, b) => a * b, 1);

  const { from, to } = connectBoxes({ junctionBoxes }).lastConnection;
  const part2 = from.position.x * to.position.x;

  console.log(`Part 1: ${part1}`);
  console.log(`Part 2: ${part2}`);
};

await main();
