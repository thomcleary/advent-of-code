"""
Day 1: Calorie Counting
"""

from dataclasses import dataclass


@dataclass
class Elf:
    number: int
    calories: int


def get_elf_with_most_calories() -> Elf:
    with open("puzzle-input.txt", "r", encoding="utf-8") as puzzle_input:
        return max(
            [
                Elf(x + 1, sum(map(int, calories)))
                for (x, calories) in enumerate(
                    [
                        calories.split("\n")
                        for calories in puzzle_input.read().split("\n\n")
                    ]
                )
            ],
            key=lambda e: e.calories,
        )


def main() -> None:
    print(get_elf_with_most_calories().calories)


if __name__ == ("__main__"):
    main()
