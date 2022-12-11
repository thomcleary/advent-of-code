"""
Day 10: Cathode-Ray Tube
"""

from dataclasses import dataclass
from typing import Optional


@dataclass
class Instruction:
    instruction: str
    cycle_time: int
    arg: Optional[int] = None


def get_instructions() -> list[Instruction]:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return [
            Instruction(line, 1) if " " not in line else Instruction(line.split()[0], 2, int(line.split()[1]))
            for line in [line.strip() for line in puzzle_input.readlines()]
        ]


def main() -> None:
    # Part 1
    register_x = 1
    cycle_num = 0
    signal_strengths: dict[int, int] = {}

    for instruction in get_instructions():
        for _ in range(instruction.cycle_time):
            cycle_num += 1
            signal_strengths[cycle_num] = cycle_num * register_x
        if instruction.arg:
            register_x += instruction.arg

    print("Part 1:", sum(map(lambda cycle: signal_strengths[cycle], list(range(20, len(signal_strengths), 40)))))


if __name__ == "__main__":
    main()
