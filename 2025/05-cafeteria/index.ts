import assert, { Assert } from "assert";
import { stdin } from "../utils.ts";

type Inventory = {
  freshRanges: { start: number; end: number }[];
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

const getFreshIngredients = (inventory: Inventory): number[] => {
  const fresh: number[] = [];

  inventory.ingredients.forEach((ingredient) => {
    for (const range of inventory.freshRanges) {
      if (ingredient >= range.start && ingredient <= range.end) {
        fresh.push(ingredient);
        break;
      }
    }
  });

  return fresh;
};

const main = async () => {
  const inventory = getInventory(await stdin());

  const freshIngredients = getFreshIngredients(inventory);

  console.log(`Part 1: ${freshIngredients.length}`);
  console.log(`Part 2: TODO`);
};

await main();
