"""
Day 1: Calorie Counting
"""

from dataclasses import dataclass


@dataclass
class Elf:
    number: int
    calories: int


def get_elves() -> list[Elf]:
    with open("puzzle-input.txt", "r", encoding="utf-8") as puzzle_input:
        return [
            Elf(x + 1, sum(map(int, calories)))
            for (x, calories) in enumerate([calories.split("\n") for calories in puzzle_input.read().split("\n\n")])
        ]


def get_elf_with_most_calories(elves: list[Elf]) -> Elf:
    return max(elves, key=lambda e: e.calories)


def main() -> None:
    elves = get_elves()

    # Part 1: Find the elf with the most calories
    print("Part 1:", get_elf_with_most_calories(elves).calories)

    # Part 2: Find the top 3 elves with the most calories
    calories = 0
    for i in range(3):
        top_elf = get_elf_with_most_calories(elves)
        calories += top_elf.calories
        elves.remove(top_elf)

    print("Part 2:", calories)


if __name__ == ("__main__"):
    main()
