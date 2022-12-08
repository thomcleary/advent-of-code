"""
Day 7: No Space Left On Device
"""

from dataclasses import dataclass, field
from typing import Optional


@dataclass
class File:
    name: str
    depth: int  # Used for __repr__
    size: int = 0
    is_dir: bool = False
    children: list["File"] = field(default_factory=list)
    parent: Optional["File"] = None

    def __repr__(self) -> str:
        title = f"{'/' if self.is_dir and self.name != '/' else ''}{self.name:10} {self.size}\n"
        contents = "".join([f"{'.' * 2 * child.depth}{child}" for child in self.children])
        return f"{title} {contents}"

    def add_file(self, child: "File") -> None:
        self.children.append(child)
        self.update_size(child.size)

    def update_size(self, size: int) -> None:
        current_dir: Optional["File"] = self
        while current_dir != None:
            assert current_dir is not None  # make mypy happy
            current_dir.size += size
            current_dir = current_dir.parent


@dataclass
class Context:
    current_dir: File


def is_command(line: str) -> bool:
    return line[0] == "$"


def cd(args: list[str], context: Context) -> None:
    target_file = args[0]

    if target_file == "..":
        assert context.current_dir.parent is not None
        context.current_dir = context.current_dir.parent
        return

    for file in context.current_dir.children:
        if file.name == target_file:
            context.current_dir = file
            return

    raise FileNotFoundError((f"{target_file} not found in {context.current_dir.name}"))


def ls(args: list[str], context: Context) -> None:
    return


COMMANDS = {"cd": cd, "ls": ls}


def read_command(command: str) -> tuple[str, list[str]]:
    _, command, *args = command.split()
    return (command, args)


def read_file_info(info: str, context: Context) -> File:
    info, name = info.split()
    if info == "dir":
        return File(name=name, is_dir=True, parent=context.current_dir, depth=context.current_dir.depth + 1)
    return File(name=name, size=int(info), parent=context.current_dir, depth=context.current_dir.depth + 1)


def get_file_system_info() -> File:
    with open("puzzle-input.txt", "r") as puzzle_input:
        lines = [line.strip() for line in puzzle_input.readlines()][1:]

    root = File(name="/", is_dir=True, depth=0)
    context = Context(current_dir=root)

    for line in lines:
        if is_command(line):
            command, args = read_command(line)
            COMMANDS[command](args, context)
        else:
            context.current_dir.add_file(read_file_info(line, context))

    return root


def get_directories_under_size(root: File, max_size: int, valid_dirs: list[File] = []) -> list[File]:
    assert root.is_dir == True
    context = Context(current_dir=root)

    if root.size <= max_size:
        valid_dirs.append(root)

    for child in context.current_dir.children:
        if child.is_dir:
            get_directories_under_size(child, max_size, valid_dirs)

    return valid_dirs


def main() -> None:
    root = get_file_system_info()

    # Part 1
    MAX_DIR_SIZE = 100_000
    print("Part 1:", sum([dir.size for dir in get_directories_under_size(root, MAX_DIR_SIZE)]))


if __name__ == "__main__":
    main()
