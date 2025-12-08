import assert from "node:assert";
import { stdin } from "../utils.ts";

type Problem = {
  operands: number[];
  operator: "*" | "+";
};

const getHomeworkProblems = (input: string): Problem[] => {
  const lines = input
    .split(/\r?\n/)
    .map((line) => line.split(/\s+/).filter((char) => !!char));

  const problemCount = lines.at(0)?.length;
  assert(problemCount !== undefined);

  const problems: Problem[] = [];

  for (let p = 0; p < problemCount; p++) {
    const operator = lines.at(-1)?.at(p);
    assert(operator === "*" || operator === "+");

    const operands = lines.slice(0, -1).map((line) => {
      const operand = line.at(p);
      assert(operand !== undefined);
      return parseInt(operand);
    });

    problems.push({
      operands,
      operator,
    });
  }

  return problems;
};

const solve = (problem: Problem) => {
  const [first, ...rest] = problem.operands;
  assert(first !== undefined);

  let result = first;

  for (const operand of rest) {
    switch (problem.operator) {
      case "*":
        result *= operand;
        break;
      case "+":
        result += operand;
        break;
    }
  }

  return result;
};

const main = async () => {
  const problems = getHomeworkProblems(await stdin());

  const answerTotal = problems.map(solve).reduce((a, b) => a + b, 0);

  console.log(`Part 1: ${answerTotal}`);
  console.log(`Part 2: TODO`);
};

await main();
