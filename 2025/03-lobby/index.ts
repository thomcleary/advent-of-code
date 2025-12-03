import assert from "node:assert";
import { stdin } from "../utils.ts";

const maxJoltage = ({ bank, batteriesRequired }: { bank: number[]; batteriesRequired: number }): number => {
  const batteries: number[] = [];
  let windowHead = 0;

  while (batteries.length < batteriesRequired) {
    const windowSize = bank.length - windowHead - (batteriesRequired - batteries.length) + 1;
    const window = bank.slice(windowHead, windowHead + windowSize);

    const largestBattery = Math.max(...window);
    const largestBatteryIndex = window.indexOf(largestBattery);
    assert(largestBatteryIndex >= 0);

    batteries.push(largestBattery);

    windowHead += largestBatteryIndex + 1;
  }

  return parseInt(batteries.map((b) => b.toString()).join(""));
};

const totalJoltage = ({ banks, batteriesRequired }: { banks: number[][]; batteriesRequired: number }) => {
  return banks.map((bank) => maxJoltage({ bank, batteriesRequired })).reduce((a, b) => a + b, 0);
};

const main = async () => {
  const banks = (await stdin()).split("\n").map((l) => l.split("").map((d) => parseInt(d)));

  console.log(`Part 1: ${totalJoltage({ banks, batteriesRequired: 2 })}`);
  console.log(`Part 2: ${totalJoltage({ banks, batteriesRequired: 12 })}`);
};

await main();
