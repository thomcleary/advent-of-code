"""
Day 9: Rope Bridge
"""

from enum import IntEnum


class Axis(IntEnum):
    X = 0
    Y = 1


def get_head_moves() -> list[tuple[Axis, int]]:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return [
            (
                Axis.X if direction in ["L", "R"] else Axis.Y,
                int(distance) if direction in ["U", "R"] else -int(distance),
            )
            for direction, distance in [line.strip().split() for line in puzzle_input.readlines()]
        ]


def move_tail(tail_pos: list[int], head_pos: list[int]) -> None:
    x_diff = head_pos[Axis.X] - tail_pos[Axis.X]
    y_diff = head_pos[Axis.Y] - tail_pos[Axis.Y]

    if abs(x_diff) <= 1 and abs(y_diff) <= 1:
        return
    elif y_diff == 0:
        tail_pos[Axis.X] += x_diff // abs(x_diff)
    elif x_diff == 0:
        tail_pos[Axis.Y] += y_diff // abs(y_diff)
    else:
        tail_pos[Axis.X] += x_diff // abs(x_diff)
        tail_pos[Axis.Y] += y_diff // abs(y_diff)


def main() -> None:
    moves = get_head_moves()

    start_pos = [0, 0]
    head_pos = start_pos
    tail_pos = start_pos.copy()

    locations_tail_visited: dict[tuple[int, int], bool] = {}

    for axis, distance in moves:
        for _ in range(abs(distance)):
            move = int(distance) // abs(distance)
            head_pos[axis] += move
            move_tail(tail_pos, head_pos)
            locations_tail_visited[tail_pos[Axis.X], tail_pos[Axis.Y]] = True

    # Part 1
    print("Part 1:", len(locations_tail_visited))


if __name__ == "__main__":
    main()
