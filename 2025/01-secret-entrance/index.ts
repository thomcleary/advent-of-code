import { getInputLines } from "../utils.ts";

const TOTAL_POSITIONS = 100;
const STARTING_POSITION = 50;

type Rotation = { type: "left" | "right"; value: number };

const getPasswords = (rotations: Rotation[]) => {
  let oldPassword = 0;
  let newPassword = 0;

  let position = STARTING_POSITION;

  for (const rotation of rotations) {
    for (let i = 0; i < rotation.value; i++) {
      if (rotation.type === "left") {
        position -= 1;
      } else {
        position += 1;
      }

      position %= TOTAL_POSITIONS;

      if (position === 0) {
        newPassword += 1;
      }
    }

    if (position === 0) {
      oldPassword += 1;
    }
  }

  return { oldPassword, newPassword };
};

const main = async () => {
  const lines = await getInputLines();

  const rotations = lines.map(
    (line) =>
      ({
        type: line.at(0) === "L" ? "left" : "right",
        value: parseInt(line.slice(1)),
      }) satisfies Rotation
  );

  const { oldPassword, newPassword } = getPasswords(rotations);

  console.log(`Part 1: ${oldPassword}`);
  console.log(`Part 2: ${newPassword}`);
};

await main();
