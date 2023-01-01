"""
Day 8: Treetop Tree House
"""


class Colour:
    BOLD = "\033[1m"
    DARK_GRAY = "\033[1;30m"
    ENDC = "\033[0m"
    GREEN = "\033[92m"
    WHITE = "\033[1;37m"
    YELLOW = "\033[93m"


def print_colour(output: str | int, colour: str = Colour.WHITE, bold=False, end="\n") -> None:
    output = colour + str(output) + Colour.ENDC
    if bold:
        output = Colour.BOLD + output
    print(output, end=end)


TreeMap = list[list[int]]


def get_tree_map() -> TreeMap:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return [[int(tree) for tree in list(trees.strip())] for trees in puzzle_input.readlines()]


def is_map_edge(row: int, col: int, tree_map: TreeMap) -> bool:
    return row == 0 or col == 0 or row == len(tree_map[0]) - 1 or col == len(tree_map) - 1


def get_tree_stats(row: int, col: int, tree_map: TreeMap) -> tuple[bool, int]:
    if is_map_edge(row, col, tree_map):
        return True, 0

    tree_map_transpose: TreeMap = list(map(list, zip(*tree_map)))
    tree = tree_map[row][col]

    west_trees = list(reversed(tree_map[row][0:col]))
    east_trees = tree_map[row][col + 1 :]
    north_trees = list(reversed(tree_map_transpose[col][:row]))
    south_trees = tree_map_transpose[col][row + 1 :]

    is_visible = tree > min(max(west_trees), max(east_trees), max(south_trees), max(north_trees))

    scenic_score = 1
    for direction in [west_trees, east_trees, north_trees, south_trees]:
        distance = 0
        for visible_tree in direction:
            distance += 1
            if visible_tree >= tree:
                break
        scenic_score *= distance

    return is_visible, scenic_score


def get_tree_map_stats(tree_map: TreeMap) -> tuple[int, int]:
    num_visible = 0
    best_scenic_score = 0

    for row, trees in enumerate(tree_map):
        for col in range(len(trees)):
            is_visible, scenic_score = get_tree_stats(row, col, tree_map)
            if scenic_score > best_scenic_score:
                best_scenic_score = scenic_score
            if is_visible:
                num_visible += 1

    return num_visible, best_scenic_score


def print_map_result(tree_map: TreeMap, max_scenic_score: int) -> None:
    print()
    for row, trees in enumerate(tree_map):
        for col, tree in enumerate(trees):
            is_visible, scenic_score = get_tree_stats(row, col, tree_map)
            if scenic_score == max_scenic_score:
                print_colour(tree, Colour.YELLOW, bold=True, end="")
            elif is_visible:
                print_colour(tree, Colour.GREEN, end="")
            else:
                print_colour(tree, Colour.DARK_GRAY, end="")
        print()
    print()


def main() -> None:
    tree_map = get_tree_map()
    num_trees_visible, best_scenic_score = get_tree_map_stats(tree_map)

    # Part 1
    print("Part 1:", num_trees_visible)

    # Part 2
    print("Part 2:", best_scenic_score)

    # Visualisation
    print_map_result(tree_map, best_scenic_score)


if __name__ == "__main__":
    main()
