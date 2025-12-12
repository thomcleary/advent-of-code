import assert from "node:assert";
import { stdin } from "../utils.ts";

type Devices = Map<string, string[]>;

const getDevices = (input: string): Devices =>
  input
    .split(/\r?\n/)
    .map((line) => {
      const [left, right] = line.split(":");

      const device = left;
      const outputs = right?.trim().split(" ");

      assert(device !== undefined);
      assert(outputs !== undefined);

      return { device, outputs };
    })
    .reduce(
      (map, { device, outputs }) => map.set(device, outputs),
      new Map<string, string[]>()
    );

const findPaths = ({
  devices,
  from,
  to,
}: {
  from: string;
  to: string;
  devices: Devices;
}): number => {
  const initialOutputs = devices.get(from);
  assert(initialOutputs !== undefined);

  const q = [...initialOutputs];
  let paths = 0;

  while (q.length > 0) {
    const output = q.shift();
    assert(output !== undefined);

    if (output === to) {
      paths += 1;
      continue;
    }

    const nextOutputs = devices.get(output);
    assert(nextOutputs !== undefined);

    for (const next of nextOutputs) {
      q.push(next);
    }
  }

  return paths;
};

const main = async () => {
  const devices = getDevices(await stdin());

  console.log(`Part 1: ${findPaths({ devices, from: "you", to: "out" })}`);
  console.log(`Part 2: TODO`);
};

await main();
