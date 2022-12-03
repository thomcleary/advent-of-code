"""
Day 2: Paper Scissors Rock
"""

from dataclasses import dataclass
from enum import Enum, auto
from typing import Callable


class Moves(Enum):
    ROCK = auto()
    PAPER = auto()
    SCISSORS = auto()


class Outcome(Enum):
    LOSS = 0
    DRAW = 3
    WIN = 6


@dataclass
class Move:
    points: int
    beats: Moves


@dataclass
class Game:
    opponent_move: Moves
    player_move: Moves
    outcome: Outcome


MOVE_INFO: dict[Moves, Move] = {
    Moves.ROCK: Move(1, Moves.SCISSORS),
    Moves.PAPER: Move(2, Moves.ROCK),
    Moves.SCISSORS: Move(3, Moves.PAPER),
}

ENCRYPTED_OPPONENT_MOVES: dict[str, Moves] = {
    "A": Moves.ROCK,
    "B": Moves.PAPER,
    "C": Moves.SCISSORS,
}

ENCRYPTED_PLAYER_MOVES: dict[str, Moves] = {
    "X": Moves.ROCK,
    "Y": Moves.PAPER,
    "Z": Moves.SCISSORS,
}

ENCRYPTED_OUTCOME: dict[str, Outcome] = {
    "X": Outcome.LOSS,
    "Y": Outcome.DRAW,
    "Z": Outcome.WIN,
}


def get_games() -> list[Game]:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return [
            Game(
                ENCRYPTED_OPPONENT_MOVES[opponent_move],
                ENCRYPTED_PLAYER_MOVES[player_strategy],
                ENCRYPTED_OUTCOME[player_strategy],
            )
            for opponent_move, player_strategy in [
                game.split(" ") for game in puzzle_input.read().split("\n")
            ]
        ]


def get_game_outcome_points(game: Game) -> int:
    if MOVE_INFO[game.player_move].beats == game.opponent_move:
        return Outcome.WIN.value
    if game.player_move == game.opponent_move:
        return Outcome.DRAW.value
    return Outcome.LOSS.value


def get_player_move_points(game: Game) -> int:
    if game.outcome == Outcome.LOSS:
        return MOVE_INFO[MOVE_INFO[game.opponent_move].beats].points
    if game.outcome == Outcome.DRAW:
        return MOVE_INFO[game.opponent_move].points
    return MOVE_INFO[
        [move for move, info in MOVE_INFO.items() if info.beats == game.opponent_move][
            0
        ]
    ].points


def get_score(get_game_points: Callable[[Game], int]) -> int:
    return sum(map(get_game_points, get_games()))


def main() -> None:
    # Part 1
    print(
        "Part 1:",
        get_score(
            lambda g: MOVE_INFO[g.player_move].points + get_game_outcome_points(g)
        ),
    )

    # Part 2
    print("Part 2:", get_score(lambda g: g.outcome.value + get_player_move_points(g)))


if __name__ == "__main__":
    main()
