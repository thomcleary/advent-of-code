import assert from "node:assert";
import { stdin } from "../utils.ts";

type Operator = "*" | "+";

type Problem = {
  operands: number[];
  operator: Operator;
};

const getHumanProblems = (input: string): Problem[] => {
  const lines = input
    .split(/\r?\n/)
    .map((line) => line.split(/\s+/).filter((str) => !!str));

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

const getCephalopodProblems = (input: string): Problem[] => {
  const lines = input.split(/\r?\n/);
  const longestLine = lines.reduce(
    (prev, curr) => Math.max(prev, curr.length),
    0
  );
  const paddedLines = lines.map((line) => line.padEnd(longestLine, " "));

  const operandLines = paddedLines.slice(0, -1);
  const operatorLine = paddedLines.at(-1);
  assert(operatorLine !== undefined);

  const problemRanges: { start: number; end: number; operator: Operator }[] =
    [];

  const firstOperator = operatorLine.at(0);
  assert(firstOperator === "*" || firstOperator === "+");

  let currentOperator: Operator = firstOperator;
  let start = 0;

  for (let i = start + 1; i < operatorLine.length; i++) {
    const char = operatorLine.at(i);
    assert(char !== undefined);

    if (char === "*" || char === "+") {
      problemRanges.push({ start, end: i - 2, operator: currentOperator });
      start = i;
      currentOperator = char;
    }
  }
  problemRanges.push({
    start,
    end: operatorLine.length - 1,
    operator: currentOperator,
  });

  const problems: Problem[] = [];

  for (const range of problemRanges) {
    const operands: number[] = [];

    const rangeOperands = operandLines.map((line) =>
      line.slice(range.start, range.end + 1)
    );

    const rangeColumns = rangeOperands[0]?.length;
    assert(rangeColumns !== undefined);

    for (let column = rangeColumns - 1; column >= 0; column--) {
      let operand = "";
      for (const line of rangeOperands) {
        operand += line[column] ?? "";
      }
      operands.push(parseInt(operand));
    }

    problems.push({
      operands,
      operator: range.operator,
    });
  }

  return problems;
};

const solve = (problems: Problem[]) => {
  return problems
    .map(({ operands, operator }) => {
      const [first, ...rest] = operands;
      assert(first !== undefined);

      let result = first;

      for (const operand of rest) {
        if (operator === "*") {
          result *= operand;
        } else {
          result += operand;
        }
      }

      return result;
    })
    .reduce((a, b) => a + b, 0);
};

const main = async () => {
  const input = await stdin();

  console.log(`Part 1: ${solve(getHumanProblems(input))}`);
  console.log(`Part 2: ${solve(getCephalopodProblems(input))}`);
};

await main();
