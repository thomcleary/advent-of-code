"""
Day 4: Cample Cleanup
"""

from dataclasses import dataclass


@dataclass
class Assignment:
    lower_id: int
    upper_id: int


def get_assignment_pairs() -> list[tuple[Assignment, ...]]:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return [
            tuple(
                [Assignment(int(lower), int(upper)) for lower, upper in [assignment.split("-") for assignment in pair]]
            )
            for pair in [pair.split(",") for pair in [line for line in puzzle_input.read().split("\n")]]
        ]


def assignment_contains_assignment(assignment: Assignment, compareTo: Assignment) -> bool:
    return assignment.lower_id <= compareTo.lower_id and assignment.upper_id >= compareTo.upper_id


def pair_has_full_assignment_overlap(pair: tuple[Assignment, ...]) -> bool:
    return assignment_contains_assignment(pair[0], pair[1]) or assignment_contains_assignment(pair[1], pair[0])


def main() -> None:
    # Part 1
    print("Part 1:", len([pair for pair in get_assignment_pairs() if pair_has_full_assignment_overlap(pair)]))


if __name__ == "__main__":
    main()
