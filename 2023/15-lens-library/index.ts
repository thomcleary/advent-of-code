import { getPuzzleInput, runPuzzle } from "../utils.js";

const useExample = false;

type Step = { label: string; operation: "-" | "="; focalLength: number };

const initialisationSequence = getPuzzleInput(import.meta.url, { useExample }).split(",");

const hash = (s: string) =>
  s.split("").reduce((total, currChar) => ((total + currChar.charCodeAt(0)) * 17) % 256, 0);

const part1 = () =>
  initialisationSequence.reduce((prevHash, currStep) => prevHash + hash(currStep), 0);

const part2 = () => {
  const boxes = new Map<number, Step[]>();

  initialisationSequence.forEach((step) => {
    const s = {
      label: step.replaceAll(/[^a-zA-Z]/g, ""),
      operation: step.replaceAll(/[^-=]/g, "") as "-" | "=",
      focalLength: Number.parseInt(step.replaceAll(/[^\d]/g, "")),
    } satisfies Step;

    const key = hash(s.label);
    const boxLenses = boxes.get(key);

    if (s.operation === "-") {
      boxLenses && boxLenses.length > 0
        ? boxes.set(
            key,
            boxLenses.filter((b) => b.label !== s.label),
          )
        : boxes.delete(key);
    } else {
      boxLenses && boxLenses?.filter((b) => b.label === s.label).length > 0
        ? (boxLenses[boxLenses.findIndex((b) => b.label === s.label)] = s)
        : boxes.set(key, boxLenses ? [...boxLenses, s] : [s]);
    }
  });

  return [...boxes.entries()]
    .map(([boxNum, lenses]) =>
      lenses
        .map((l, slotNum) => (boxNum + 1) * (slotNum + 1) * l.focalLength)
        .reduce((p, c) => p + c, 0),
    )
    .reduce((p, c) => p + c, 0);
};

/**
 * @description https://adventofcode.com/2023/day/15
 */
runPuzzle({
  day: 15,
  name: "Lens Library",
  part1: { run: part1, expected: useExample ? 1320 : 514394 },
  part2: { run: part2, expected: useExample ? 145 : 236358 },
});
