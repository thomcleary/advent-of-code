"""
Day 4: Camp Cleanup
"""

from dataclasses import dataclass
from typing import Callable


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
            for pair in [line.split(",") for line in puzzle_input.read().split("\n")]
        ]


def assignment_contains_assignment(assignment: Assignment, compareTo: Assignment) -> bool:
    return assignment.lower_id <= compareTo.lower_id and assignment.upper_id >= compareTo.upper_id


def that_have_overlap(pair: tuple[Assignment, ...]) -> bool:
    assignment, compareTo = pair[0], pair[1]
    return (not assignment.lower_id > compareTo.upper_id) and (
        assignment.lower_id >= compareTo.lower_id or assignment.upper_id >= compareTo.lower_id
    )


def that_have_full_overlap(pair: tuple[Assignment, ...]) -> bool:
    return assignment_contains_assignment(pair[0], pair[1]) or assignment_contains_assignment(pair[1], pair[0])


def number_of_pairs(pair_filter: Callable[[tuple[Assignment, ...]], bool]) -> int:
    return len([pair for pair in get_assignment_pairs() if pair_filter(pair)])


def main() -> None:
    # Part 1
    print("Part 1:", number_of_pairs(that_have_full_overlap))

    # Part 2
    print("Part 2:", number_of_pairs(that_have_overlap))


if __name__ == "__main__":
    main()
