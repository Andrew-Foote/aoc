from enum import Enum
from typing import Iterator

test_inputs = [('example', '''\
A Y
B X
C Z\
''', [
    ('p1_csv', '\n'.join(['paper,rock,win,8', 'rock,paper,lose,1', 'scissors,scissors,draw,6'])),
    ('p1', 15),
    ('p2_csv', '\n'.join(['rock,rock,draw,4', 'rock,paper,lose,1', 'rock,scissors,win,7'])),
    ('p2', 12)
])]

def csv_p1(ip: str) -> None:
    return '\n'.join(
        ','.join((*round_, ROUND_TO_OUTCOME[round_], round_to_score(*round_)))
        for round_ in p1_interpret_guide(ip)
    )

class Move(Enum):
    ROCK = 'rock'
    PAPER = 'paper'
    SCISSORS = 'scissors'

class Outcome(Enum):
    WIN = 'win'
    LOSE = 'lose'
    DRAW = 'draw'

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

def parse_guide(ip: str) -> Iterator[tuple[str, str]]:
    rounds = filter(None, (s.strip() for s in ip.split('\n')))

    for round_ in rounds:
        col1_code, col2_code = round_.split()
        yield col1_code, col2_code

def p1_interpret_guide(ip: str) -> Iterator[Round]:
    for col1_code, col2_code in parse_guide(ip):
        other_move = COL1_CODE_TO_MOVE[col1_code]
        self_move = COL2_CODE_TO_MOVE[col2_code]
        yield self_move, other_move

def round_to_score(self_move: Move, other_move: Move) -> int:
    return (
        MOVE_TO_SCORE[self_move]
        + OUTCOME_TO_SCORE[ROUND_TO_OUTCOME[self_move, other_move]]
    )

def p1_csv(ip: str) -> None:
    return '\n'.join(
        ','.join((
            round_[0].value, round_[1].value,
            ROUND_TO_OUTCOME[round_].value, str(round_to_score(*round_)))
        )
        for round_ in p1_interpret_guide(ip)
    )

def p1(ip: str) -> int:
    return str(sum(round_to_score(*round_) for round_ in p1_interpret_guide(ip)))

COL2_CODE_TO_OUTCOME: dict[str, Outcome] = {
    'X': Outcome.LOSE,
    'Y': Outcome.DRAW,
    'Z': Outcome.WIN
}

OTHER_MOVE_AND_OUTCOME_TO_SELF_MOVE: dict[tuple[Move, Outcome], Move] = {
    (other_move, outcome): self_move
    for (self_move, other_move), outcome in ROUND_TO_OUTCOME.items()
}

def p2_interpret_guide(ip: str) -> Iterator[Round]:
    for col1_code, col2_code in parse_guide(ip):
        other_move = COL1_CODE_TO_MOVE[col1_code]
        outcome = COL2_CODE_TO_OUTCOME[col2_code]
        self_move = OTHER_MOVE_AND_OUTCOME_TO_SELF_MOVE[other_move, outcome]
        yield self_move, other_move

def p2_csv(ip: str) -> None:
    return '\n'.join(
        ','.join((
            round_[0].value, round_[1].value,
            ROUND_TO_OUTCOME[round_].value, str(round_to_score(*round_)))
        )
        for round_ in p2_interpret_guide(ip)
    )

def p2(ip: str) -> int:
    return str(sum(round_to_score(*round_) for round_ in p2_interpret_guide(ip)))