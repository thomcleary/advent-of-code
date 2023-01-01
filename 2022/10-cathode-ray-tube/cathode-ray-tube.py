"""
Day 10: Cathode-Ray Tube
"""
from enum import StrEnum
from dataclasses import dataclass
from typing import Optional


class Colour(StrEnum):
    BOLD = "\033[1m"
    DARK_GRAY = "\033[1;30m"
    ENDC = "\033[0m"
    GREEN = "\033[92m"


def print_colour(output: str | int, colour: Colour, bold=False, end="\n") -> None:
    output = colour + str(output) + Colour.ENDC
    if bold:
        output = Colour.BOLD + output
    print(output, end=end)


class Pixel(StrEnum):
    On = "#"
    Off = "."


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
    instructions = get_instructions()

    # Part 1
    register_x = 1
    cycle_num = 0
    signal_strengths: dict[int, int] = {}

    for instruction in instructions:
        for _ in range(instruction.cycle_time):
            cycle_num += 1
            signal_strengths[cycle_num] = cycle_num * register_x
        if instruction.arg:
            register_x += instruction.arg

    print("Part 1:", sum(map(lambda cycle: signal_strengths[cycle], list(range(20, len(signal_strengths), 40)))))

    # Part 2
    SCREEN_WIDTH = 40
    SCREEN_HEIGHT = 6
    screen = [list(Pixel.Off * SCREEN_WIDTH) for _ in range(SCREEN_HEIGHT)]

    register_x = 1
    cycle_num = 0

    for instruction in instructions:
        for _ in range(instruction.cycle_time):
            cycle_num += 1
            curr_screen_row = (cycle_num - 1) // SCREEN_WIDTH % SCREEN_HEIGHT
            curr_pixel = (cycle_num - 1) % SCREEN_WIDTH

            if curr_pixel in [register_x - 1, register_x, register_x + 1]:
                screen[curr_screen_row][curr_pixel] = Pixel.On

        if instruction.arg:
            register_x += instruction.arg

    print("Part 2:")
    for line in screen:
        for pixel in line:
            if pixel == Pixel.On:
                print_colour(Pixel.On, Colour.GREEN, bold=True, end="")
            elif pixel == Pixel.Off:
                print_colour(Pixel.Off, Colour.DARK_GRAY, end="")
        print()


if __name__ == "__main__":
    main()
