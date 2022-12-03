"""
Day 2: Paper Scissors Rock
"""

from dataclasses import dataclass
from enum import Enum, auto


class Moves(Enum):
    ROCK = auto()
    PAPER = auto()
    SCISSORS = auto()


class OutcomePoints(Enum):
    LOSS = 0
    DRAW = 3
    WIN = 6


@dataclass
class Move:
    points: int
    beats: Moves


@dataclass
class Game:
    player_move: Moves
    opponent_move: Moves


MOVE_INFO: dict[Moves, Move] = {
    Moves.ROCK: Move(1, Moves.SCISSORS),
    Moves.PAPER: Move(2, Moves.ROCK),
    Moves.SCISSORS: Move(3, Moves.PAPER),
}

ENCRYPTED_MOVES: dict[str, Moves] = {
    "A": Moves.ROCK,
    "B": Moves.PAPER,
    "C": Moves.SCISSORS,
    "X": Moves.ROCK,
    "Y": Moves.PAPER,
    "Z": Moves.SCISSORS,
}


def get_games() -> list[Game]:
    with open("puzzle-input.txt", "r") as puzzle_input:
        return [
            Game(ENCRYPTED_MOVES[player_move], ENCRYPTED_MOVES[opponent_move])
            for opponent_move, player_move in [
                game.split(" ") for game in puzzle_input.read().split("\n")
            ]
        ]


def get_game_outcome_points(game: Game) -> int:
    if MOVE_INFO[game.player_move].beats == game.opponent_move:
        return OutcomePoints.WIN.value
    if game.player_move == game.opponent_move:
        return OutcomePoints.DRAW.value
    return OutcomePoints.LOSS.value


def get_score(game: Game) -> int:
    return MOVE_INFO[game.player_move].points + get_game_outcome_points(game)


def main() -> None:
    # Part 1: Find the total score if playing exactly as specified in strategy guide
    print("Part 1: ", sum(map(get_score, get_games())))


if __name__ == "__main__":
    main()
