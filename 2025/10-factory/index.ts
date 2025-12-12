import assert from "assert";
import { stdin } from "../utils.ts";

type Button = {
  id: number;
  toggles: number[];
};

type Machine = {
  lights: boolean[];
  buttons: Button[];
  joltages: number[];
};

type State = {
  lights: boolean[];
  button: Button;
  presses: number;
};

const getMachines = (input: string): Machine[] =>
  input.split(/\r?\n/).map((line) => {
    const parts = line.split(" ");

    const lights = parts
      .at(0)
      ?.slice(1, -1)
      .split("")
      .map((light) => light === "#");

    const buttons = parts.slice(1, -1).map((button, i) => ({
      id: i,
      toggles: button
        .slice(1, -1)
        .split(",")
        .map((n) => parseInt(n)),
    }));

    const joltages = parts
      .at(-1)
      ?.slice(1, -1)
      .split(",")
      .map((joltage) => parseInt(joltage));

    assert(lights !== undefined);
    assert(buttons !== undefined);
    assert(joltages !== undefined);

    return { lights, buttons, joltages } satisfies Machine;
  });

const configureLights = (machine: Machine): number => {
  const q: State[] = machine.buttons.map((button) => ({
    lights: machine.lights.map((_) => false),
    button,
    presses: 0,
  }));

  const seen = new Set<string>();

  while (q.length > 0) {
    const next = q.shift();
    assert(next !== undefined);

    const seenKey = `${next.lights}${next.button.id}`;
    if (seen.has(seenKey)) {
      continue;
    }
    seen.add(seenKey);

    for (const toggle of next.button.toggles) {
      const light = next.lights.at(toggle);
      assert(light !== undefined);
      next.lights[toggle] = !light;
    }

    next.presses += 1;

    // Should probably use bits to represent lights
    if (machine.lights.toString() === next.lights.toString()) {
      return next.presses;
    }

    for (const button of machine.buttons) {
      if (button.id !== next.button.id) {
        q.push({
          lights: [...next.lights],
          button,
          presses: next.presses,
        });
      }
    }
  }

  throw new Error("Failed to find correctly configured state");
};

const main = async () => {
  const machines = getMachines(await stdin());

  const fewestPressesToConfigureLights = machines
    .map(configureLights)
    .reduce((a, b) => a + b);

  console.log(`Part 1: ${fewestPressesToConfigureLights}`);
  console.log(`Part 2: TODO`);
};

await main();
