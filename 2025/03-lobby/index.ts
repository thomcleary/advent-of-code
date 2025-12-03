import { stdin } from "../utils.ts";

const maxJoltage = (bank: number[]): number => {
  const { first, second } = bank.reduce(
    ({ first, second }, curr, index) => {
      if (curr > first && index !== bank.length - 1) {
        return { first: curr, second: 0 };
      }

      if (curr > second) {
        return { first, second: curr };
      }

      return { first, second };
    },
    { first: 0, second: 0 }
  );

  return parseInt(`${first}${second}`);
};

const main = async () => {
  const banks = (await stdin()).split("\n").map((l) => l.split("").map((d) => parseInt(d)));

  const totalJoltageOutput = banks.map(maxJoltage).reduce((a, b) => a + b, 0);

  console.log(`Part 1: ${totalJoltageOutput}`);
  console.log(`Part 2: TODO`);
};

await main();
