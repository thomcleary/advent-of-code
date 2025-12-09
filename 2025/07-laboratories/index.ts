import assert from "node:assert";
import { stdin } from "../utils.ts";

type Position = { row: number; column: number };
type Marker = "S" | "^" | "." | "|";

class Diagram {
  #original: readonly Marker[][];
  #diagram: Marker[][] = [];

  constructor(input: string) {
    const chars = input.split(/\r?\n/).map((line) => line.split(""));
    this.#assertIsMarkers(chars);
    this.#original = chars;
    this.reset();
  }

  get size() {
    return {
      rows: this.#diagram.length,
      columns: this.#diagram.at(0)?.length ?? 0,
    };
  }

  get startPosition(): Position {
    for (const [rowIndex, row] of this.#diagram.entries()) {
      for (const [colIndex, col] of row.entries()) {
        if (col === "S") {
          return { row: rowIndex, column: colIndex };
        }
      }
    }

    throw new Error("'S' marker not found");
  }

  at(position: Position) {
    const row = this.#diagram.at(position.row);
    assert(row !== undefined);

    const column = row[position.column];
    assert(column !== undefined);

    return column;
  }

  update(position: Position, marker: Marker) {
    const row = this.#diagram.at(position.row);
    assert(row !== undefined);
    row[position.column] = marker;
  }

  reset() {
    this.#diagram = [];
    for (const row of this.#original) {
      const rowCopy = [];
      for (const column of this.#original) {
        rowCopy.push(column);
      }
      this.#diagram.push(row);
    }
  }

  #assertIsMarkers(chars: string[][]): asserts chars is Marker[][] {
    for (const row of chars) {
      for (const column of row) {
        const markers: string[] = ["S", "^", "."] satisfies Exclude<
          Marker,
          "|"
        >[];
        assert(markers.includes(column));
      }
    }
  }
}

const countBeamSplits = (diagram: Diagram): number => {
  const queue: Position[] = [diagram.startPosition];

  let totalSplits = 0;

  while (queue.length > 0) {
    const currPos = queue.shift();
    assert(currPos !== undefined);
    assert(currPos.row < diagram.size.rows);

    if (currPos.row === diagram.size.rows - 1) {
      continue;
    }

    const nextPos = {
      row: currPos.row + 1,
      column: currPos.column,
    } satisfies Position;

    const nextMarker = diagram.at(nextPos);
    assert(nextMarker !== undefined);

    if (nextMarker === ".") {
      diagram.update(nextPos, "|");
      queue.push(nextPos);
    } else if (nextMarker === "^") {
      const nextPositions = [
        { row: nextPos.row, column: nextPos.column - 1 },
        { row: nextPos.row, column: nextPos.column + 1 },
      ] satisfies Position[];

      let splits = 0;

      for (const position of nextPositions) {
        if (diagram.at(position) === ".") {
          diagram.update(position, "|");
          splits += 1;
          queue.push(position);
        }
      }

      if (splits > 0) {
        totalSplits += 1;
      }
    }
  }

  return totalSplits;
};

const main = async () => {
  const diagram = new Diagram(await stdin());

  console.log(`Part 1: ${countBeamSplits(diagram)}`);
  console.log(`Part 2: TODO`);
};

await main();
