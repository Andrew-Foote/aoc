from enum import Enum
from typing import Iterator

class Move(Enum):
    ROCK = 0
    PAPER = 1
    SCISSORS = 2

class Outcome(Enum):
    WIN = 0
    LOSE = 1
    DRAW = 2

Round = tuple[Move, Move]

ROUND_TO_OUTCOME: dict[Round, Outcome] = {
    (Move.ROCK, Move.ROCK): Outcome.DRAW,
    (Move.ROCK, Move.PAPER): Outcome.LOSE,
    (Move.ROCK, Move.SCISSORS): Outcome.WIN,
    (Move.PAPER, Move.ROCK): Outcome.WIN,
    (Move.PAPER, Move.PAPER): Outcome.DRAW,
    (Move.PAPER, Move.SCISSORS): Outcome.LOSE,
    (Move.SCISSORS, Move.ROCK): Outcome.LOSE,
    (Move.SCISSORS, Move.PAPER): Outcome.WIN,
    (Move.SCISSORS, Move.SCISSORS): Outcome.DRAW,
}

MOVE_ATTRS: dict[Move, tuple[str, str, int]] = {
    Move.ROCK: ('A', 'X', 1),
    Move.PAPER: ('B', 'Y', 2),
    Move.SCISSORS: ('C', 'Z', 3),
}

MOVE_TO_COL1_CODE = {k: v[0] for k, v in MOVE_ATTRS.items()}
COL1_CODE_TO_MOVE = {v: k for k, v in MOVE_TO_COL1_CODE.items()}

MOVE_TO_COL2_CODE = {k: v[1] for k, v in MOVE_ATTRS.items()}
COL2_CODE_TO_MOVE = {v: k for k, v in MOVE_TO_COL2_CODE.items()}

MOVE_TO_SCORE = {k: v[2] for k, v in MOVE_ATTRS.items()}

OUTCOME_TO_SCORE: dict[Outcome, int] = {
    Outcome.WIN: 6,
    Outcome.LOSE: 0,
    Outcome.DRAW: 3
}

def parse_guide(ip: str) -> Iterator[Round]:
    rounds = filter(None, (s.strip() for s in ip.split('\n')))

    for round_ in rounds:
        col1_code, col2_code = round_.split()
        other_move = COL1_CODE_TO_MOVE[col1_code]
        self_move = COL2_CODE_TO_MOVE[col2_code]
        yield self_move, other_move

def round_to_score(self_move: Move, other_move: Move) -> int:
    return (
        MOVE_TO_SCORE[self_move]
        + OUTCOME_TO_SCORE[ROUND_TO_OUTCOME[self_move, other_move]]
    )

TEST_INPUT = '''\
A Y
B X
C Z\
'''

def test() -> None:
    rounds = list(parse_guide(TEST_INPUT))
    assert len(rounds) == 3

    assert rounds[0] == (Move.PAPER, Move.ROCK)
    assert ROUND_TO_OUTCOME[rounds[0]] == Outcome.WIN
    assert round_to_score(*rounds[0]) == 8

    assert rounds[1] == (Move.ROCK, Move.PAPER)
    assert ROUND_TO_OUTCOME[rounds[1]] == Outcome.LOSE
    assert round_to_score(*rounds[1]) == 1

    assert rounds[2] == (Move.SCISSORS, Move.SCISSORS)
    assert ROUND_TO_OUTCOME[rounds[2]] == Outcome.DRAW
    assert round_to_score(*rounds[2]) == 6

    assert sum(round_to_score(*round_) for round_ in rounds) == 15

def p1(ip: str) -> int:
    rounds = list(parse_guide(ip))
    return sum(round_to_score(*round_) for round_ in rounds)
