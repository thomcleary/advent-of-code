"""
Day 9: Rope Bridge
"""

from dataclasses import dataclass, field
from enum import IntEnum
from typing import Optional


class Axis(IntEnum):
    X = 0
    Y = 1


@dataclass
class Knot:
    parent: Optional["Knot"]
    child: Optional["Knot"]
    position: list[int]
    visited: dict[tuple[int, int], bool] = field(default_factory=dict)

    def move_as_head(self, move: tuple[Axis, int]):
        axis, distance = move
        for _ in range(abs(distance)):
            direction = int(distance) // abs(distance)
            self.position[axis] += direction
            if self.child:
                self.child.move_as_child()
            self.visited[self.position[Axis.X], self.position[Axis.Y]] = True

    def move_as_child(self):
        x_diff = self.parent.position[Axis.X] - self.position[Axis.X]
        y_diff = self.parent.position[Axis.Y] - self.position[Axis.Y]

        if abs(x_diff) <= 1 and abs(y_diff) <= 1:
            return
        elif y_diff == 0:
            self.position[Axis.X] += x_diff // abs(x_diff)
        elif x_diff == 0:
            self.position[Axis.Y] += y_diff // abs(y_diff)
        else:
            self.position[Axis.X] += x_diff // abs(x_diff)
            self.position[Axis.Y] += y_diff // abs(y_diff)

        self.visited[self.position[Axis.X], self.position[Axis.Y]] = True

        if self.child:
            self.child.move_as_child()


@dataclass
class Rope:
    head: Knot
    tail: Knot


def get_head_moves() -> list[tuple[Axis, int]]:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return [
            (
                Axis.X if direction in ["L", "R"] else Axis.Y,
                int(distance) if direction in ["U", "R"] else -int(distance),
            )
            for direction, distance in [line.strip().split() for line in puzzle_input.readlines()]
        ]


def get_rope(num_knots) -> Rope:
    START_POS = [0, 0]
    knots = [Knot(None, None, START_POS.copy()) for _ in range(num_knots)]
    for i, knot in enumerate(knots):
        knot.visited[START_POS[Axis.X], START_POS[Axis.Y]] = True
        if i != 0:
            knot.parent = knots[i - 1]
        if i != len(knots) - 1:
            knot.child = knots[i + 1]

    return Rope(knots[0], knots[-1])


def main() -> None:
    moves = get_head_moves()

    # Part 1
    KNOTS_IN_ROPE = 2
    rope = get_rope(KNOTS_IN_ROPE)

    for move in moves:
        rope.head.move_as_head(move)

    print("Part 1:", len(rope.tail.visited))

    # Part 1
    KNOTS_IN_ROPE = 10
    rope = get_rope(KNOTS_IN_ROPE)

    for move in moves:
        rope.head.move_as_head(move)

    print("Part 2:", len(rope.tail.visited))


if __name__ == "__main__":
    main()
