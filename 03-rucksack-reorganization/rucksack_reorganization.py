"""
Day 3: Rucksack Reorganization
"""

from dataclasses import dataclass


@dataclass
class Rucksack:
    leftCompartment: str
    rightCompartment: str


def get_rucksack(rucksack: str) -> Rucksack:
    items_per_compartment = len(rucksack) // 2
    return Rucksack(rucksack[:items_per_compartment], rucksack[items_per_compartment:])


def get_rucksacks() -> list[Rucksack]:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return [get_rucksack(rucksack) for rucksack in puzzle_input.read().split("\n")]


def get_item_type_in_both_compartments(rucksack: Rucksack) -> str:
    item_in_both_compartments = set(rucksack.leftCompartment).intersection(
        set(rucksack.rightCompartment)
    )

    if len(item_in_both_compartments) > 1:
        raise Exception("Found more than 1 item that is in both compartments")

    return item_in_both_compartments.pop()


def get_item_type_priority(item_type: str) -> int:
    item_a, item_a_priority = ("A", 27) if item_type.upper() == item_type else ("a", 1)
    ordDiff = ord(item_a) - item_a_priority
    return ord(item_type) - ordDiff


def main() -> None:
    # Part 1
    print(
        "Part 1:",
        sum(
            map(
                get_item_type_priority,
                map(get_item_type_in_both_compartments, get_rucksacks()),
            )
        ),
    )


if __name__ == "__main__":
    main()
