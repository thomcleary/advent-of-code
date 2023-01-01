"""
Day 3: Rucksack Reorganization
"""

from dataclasses import dataclass
from typing import Iterator, Callable


@dataclass
class Rucksack:
    contents: str
    left_compartment: str
    right_compartment: str


def get_rucksack(rucksack: str) -> Rucksack:
    items_per_compartment = len(rucksack) // 2
    return Rucksack(rucksack, rucksack[:items_per_compartment], rucksack[items_per_compartment:])


def get_rucksacks() -> list[Rucksack]:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return list(map(get_rucksack, puzzle_input.read().split("\n")))


def get_item_type_in_both_compartments(rucksack: Rucksack) -> str:
    item_in_both_compartments = set(rucksack.left_compartment).intersection(set(rucksack.right_compartment))

    if len(item_in_both_compartments) > 1:
        raise Exception("Found more than 1 item that is in both compartments")

    return item_in_both_compartments.pop()


def get_item_type_in_all_rucksacks(rucksacks: list[Rucksack]) -> str:
    item_in_all_rucksacks: set[str] = set.intersection(*(map(lambda r: set(r.contents), rucksacks)))

    if len(item_in_all_rucksacks) > 1:
        raise Exception("Found more than 1 item in the group's rucksacks")

    return item_in_all_rucksacks.pop()


def get_item_type_priority(item_type: str) -> int:
    item_a, item_a_priority = ("A", 27) if item_type.upper() == item_type else ("a", 1)
    ord_diff = ord(item_a) - item_a_priority
    return ord(item_type) - ord_diff


def groups(rucksacks: list[Rucksack]) -> Iterator[list[Rucksack]]:
    group_size = 3
    for i in range(0, len(rucksacks), group_size):
        yield rucksacks[i : i + group_size]


def main() -> None:
    # Part 1
    print(
        "Part 1:",
        sum(
            map(
                lambda r: get_item_type_priority(get_item_type_in_both_compartments(r)),
                get_rucksacks(),
            ),
        ),
    )

    # Part 2
    print(
        "Part 2:",
        sum(
            map(
                lambda g: get_item_type_priority(get_item_type_in_all_rucksacks(g)),
                list(groups(get_rucksacks())),
            ),
        ),
    )


if __name__ == "__main__":
    main()
