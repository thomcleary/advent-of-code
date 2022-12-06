"""
Day 5: Supply Stacks
"""

from dataclasses import dataclass
from typing import Callable


@dataclass
class Command:
    move_count: int
    move_from: int
    move_to: int


def get_stacks(stack_lines: list[str]) -> list[list[str]]:
    number_of_stacks = max(map(int, stack_lines[-1].split()))
    crate_width = len(stack_lines[0]) // number_of_stacks

    stack_rows: list[list[str]] = []
    for line in stack_lines[:-1]:
        line = line.strip()
        stack_rows.append(
            [line[i : i + crate_width].strip() for i in range(0, crate_width * number_of_stacks, crate_width)]
        )

    stacks: list[list[str]] = [[] for _ in range(number_of_stacks)]
    for row in list(reversed(stack_rows)):
        for crate_num, crate in enumerate(row):
            if crate != "":
                stacks[crate_num].append(crate.replace("[", "").replace("]", ""))

    return stacks


def get_stacks_and_commands() -> tuple[list[list[str]], list[Command]]:
    with open("puzzle-input.txt", "r") as puzzle_input:
        stack_finished = False
        stack_lines = []
        commands = []
        for line in puzzle_input.readlines():
            if line == "\n":
                stack_finished = True
            elif not stack_finished:
                stack_lines.append(line)
            else:
                commands.append(
                    [Command(c, f, t) for c, f, t in [list(map(int, filter(str.isnumeric, line.split())))]][0]
                )
        return (get_stacks(stack_lines), commands)


def cratemover_9000(stacks: list[list[str]], commands: list[Command]) -> None:
    for command in commands:
        for _ in range(command.move_count):
            stacks[command.move_to - 1].append(stacks[command.move_from - 1].pop())


def cratemover_9001(stacks: list[list[str]], commands: list[Command]) -> None:
    for command in commands:
        crates_to_move = stacks[command.move_from - 1][-command.move_count :]
        stacks[command.move_to - 1] += crates_to_move
        stacks[command.move_from - 1] = stacks[command.move_from - 1][: -command.move_count]


def get_top_of_stacks(move_with: Callable[[list[list[str]], list[Command]], None]) -> str:
    stacks, commands = get_stacks_and_commands()
    move_with(stacks, commands)
    return "".join([stack.pop() for stack in stacks])


def main() -> None:
    # Part 1
    print("Part 1:", get_top_of_stacks(cratemover_9000))

    # Part 2
    print("Part 1:", get_top_of_stacks(cratemover_9001))


if __name__ == "__main__":
    main()
