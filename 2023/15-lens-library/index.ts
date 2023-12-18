import { getPuzzleInput, logChallenge } from "../utils.js";

const useExample = false;

const initialisationSequence = getPuzzleInput(import.meta.url, { useExample }).split(",");

const part1 = () =>
  initialisationSequence.reduce(
    (prevHash, currSeq) =>
      prevHash + currSeq.split("").reduce((total, currChar) => ((total + currChar.charCodeAt(0)) * 17) % 256, 0),
    0,
  );

const part2 = () => {};

const lensLibrary = () =>
  logChallenge({
    name: "Day 15: Lens Library",
    part1: { run: part1, expected: useExample ? 1320 : 514394 },
    part2: { run: part2, expected: useExample ? undefined : undefined },
  });

lensLibrary();
