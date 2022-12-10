"""
Day 8: Treetop Tree House
"""


class Colour:
    HEADER = "\033[95m"
    OKBLUE = "\033[94m"
    OKCYAN = "\033[96m"
    OKGREEN = "\033[92m"
    WARNING = "\033[93m"
    FAIL = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"
    LIGHT_GRAY = "\033[0;37m"
    DARK_GRAY = "\033[1;30m"
    FAINT = "\033[2m"


TEST_INPUT = [
    [int(tree) for tree in list(trees)]
    for trees in """
30373
25512
65332
33549
35390
""".strip().split()
]

TreeMap = list[list[int]]


def print_colour(output: str | int, colour: str, bold=False, end="\n") -> None:
    output = colour + str(output) + Colour.ENDC
    if bold:
        output = Colour.BOLD + output
    print(output, end=end)


def get_tree_map() -> TreeMap:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return [[int(tree) for tree in list(trees.strip())] for trees in puzzle_input.readlines()]


def is_map_edge(row: int, col: int, tree_map: TreeMap) -> bool:
    return row == 0 or col == 0 or row == len(tree_map[0]) - 1 or col == len(tree_map) - 1


def is_visible(row: int, col: int, tree_map: TreeMap) -> bool:
    tree_map_transpose: TreeMap = list(map(list, zip(*tree_map)))
    tree = tree_map[row][col]

    if is_map_edge(row, col, tree_map):
        return True

    west_tree_max = max(tree_map[row][0:col])
    east_tree_max = max(tree_map[row][col + 1 :])
    north_tree_max = max(tree_map_transpose[col][:row])
    south_tree_max = max(tree_map_transpose[col][row + 1 :])

    return tree > west_tree_max or tree > east_tree_max or tree > north_tree_max or tree > south_tree_max


def find_num_visibile(tree_map: TreeMap) -> int:
    num_visible = 0

    print()
    for row, trees in enumerate(tree_map):
        for col, tree in enumerate(trees):
            if is_visible(row, col, tree_map):
                print_colour(tree, Colour.OKGREEN, True, end="")
                num_visible += 1
            else:
                print_colour(tree, Colour.FAINT, end="")
        print()
    print()

    return num_visible


def main() -> None:
    print("Part 1:", find_num_visibile(get_tree_map()))


if __name__ == "__main__":
    main()
