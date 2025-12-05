import assert, { Assert } from "assert";
import { stdin } from "../utils.ts";

type Range = { start: number; end: number };

type Inventory = {
  freshRanges: Range[];
  ingredients: number[];
};

const getInventory = (input: string): Inventory => {
  const [ranges, ingredients] = input.split("\n\n");

  const freshRanges = ranges?.split("\n").map((r) => {
    const [start, end] = r.split("-").map((n) => parseInt(n));
    assert(start !== undefined && end !== undefined);
    return { start, end };
  });

  const ingredientIds = ingredients?.split("\n").map((n) => parseInt(n));

  assert(freshRanges && ingredientIds);

  return { freshRanges, ingredients: ingredientIds };
};

const getFreshIngredients = (inventory: Inventory): number => {
  const fresh: number[] = [];

  inventory.ingredients.forEach((ingredient) => {
    for (const range of inventory.freshRanges) {
      if (ingredient >= range.start && ingredient <= range.end) {
        fresh.push(ingredient);
        break;
      }
    }
  });

  return fresh.length;
};

const getFreshIds = (inventory: Inventory): number => {
  const mergedRanges: Range[] = [];

  const sortedFreshRanges = inventory.freshRanges.sort((a, b) => a.start - b.start);

  for (const range of sortedFreshRanges) {
    const previousRange = mergedRanges.at(-1);

    if (!previousRange || range.start > previousRange.end) {
      mergedRanges.push(range);
    } else {
      previousRange.end = Math.max(range.end, previousRange.end);
    }
  }

  let freshIds = 0;

  for (const range of mergedRanges) {
    freshIds += range.end - range.start + 1;
  }

  return freshIds;
};

const main = async () => {
  const inventory = getInventory(await stdin());

  console.log(`Part 1: ${getFreshIngredients(inventory)}`);
  console.log(`Part 2: ${getFreshIds(inventory)}`);
};

await main();
