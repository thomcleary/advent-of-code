import { getPuzzleInput } from "../utils.js";

const puzzleInput = await getPuzzleInput(import.meta.url);

console.log(puzzleInput.split("\n").filter((s) => s !== "b"));
