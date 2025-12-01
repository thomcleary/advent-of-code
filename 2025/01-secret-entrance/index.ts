const TOTAL_POSITIONS = 100;
const STARTING_POSITION = 50;

const readStdin = async () => {
  let chunks = [];

  for await (const chunk of process.stdin) {
    chunks.push(chunk);
  }

  return chunks.join("");
};

const toRotation = (line: string) => {
  const [direction] = line;

  let rotation = parseInt(line.slice(1));

  if (direction === "L") {
    rotation *= -1;
  }

  return rotation;
};

const getPassword = (rotations: number[]) => {
  let password = 0;
  let position = STARTING_POSITION;

  for (const rotation of rotations) {
    position = (position + rotation) % TOTAL_POSITIONS;

    if (position === 0) {
      password += 1;
    }
  }

  return password;
};

const main = async () => {
  const stdin = await readStdin();
  const input = stdin.split("\n").map((l) => l.trim());

  const rotations = input.map(toRotation);

  const password = getPassword(rotations);

  console.log(`Part 1: ${password}`);
};

await main();
