import { readFile } from "node:fs/promises";
import path from "path";
import { fileURLToPath } from "url";

export const getPuzzleInput = async (moduleUrl: string) => {
  const fileName = "puzzle-input.txt";
  const dirPath = path.dirname(fileURLToPath(moduleUrl));

  const filePath = path.join(dirPath, fileName);

  return (await readFile(filePath)).toString();
};
