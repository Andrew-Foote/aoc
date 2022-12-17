from enum import Enum
import itertools as it
from typing import Iterable, Iterator, NamedTuple, Self, Type

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

class Move(Enum):
    ROCK = 'rock'
    PAPER = 'paper'
    SCISSORS = 'scissors'

    @property
    def attrs(self: Self) -> 'MoveAttrs':
        return MOVE_ATTRS[self]

MoveAttrs = NamedTuple('MoveAttrs', [
    ('beats', Move),
    ('score', int),
    ('col1_code', str),
    ('col2_code', str)
])

MOVE_ATTRS: dict[Move, MoveAttrs] = {
    Move.ROCK: MoveAttrs(Move.SCISSORS, 1, 'A', 'X'),
    Move.PAPER: MoveAttrs(Move.ROCK, 2, 'B', 'Y'),
    Move.SCISSORS: MoveAttrs(Move.PAPER, 3, 'C', 'Z')
}

class Outcome(Enum):
    WIN = 'win'
    LOSE = 'lose'
    DRAW = 'draw'

    @property
    def attrs(self: Self) -> 'OutcomeAttrs':
        return OUTCOME_ATTRS[self]

OutcomeAttrs = NamedTuple('OutcomeAttrs', [
    ('score', int),
    ('col2_code', str)
])

OUTCOME_ATTRS: dict[Outcome, OutcomeAttrs] = {
    Outcome.LOSE: OutcomeAttrs(0, 'X'),
    Outcome.DRAW: OutcomeAttrs(3, 'Y'),
    Outcome.WIN: OutcomeAttrs(6, 'Z')
}

Round = tuple[Move, Move]

ROUND_OUTCOME: dict[Round, Outcome] = dict(it.chain.from_iterable((
    ((move.attrs.beats, move), Outcome.LOSE),
    ((move, move), Outcome.DRAW),
    ((move, move.attrs.beats), Outcome.WIN)
) for move in Move))

def guide(ip: str) -> Iterator[tuple[str, str]]:
    for line in ip.splitlines():
        col1_code, col2_code = line.split()
        yield col1_code, col2_code

COL1_CODE_MOVE: dict[str, Move] = {move.attrs.col1_code: move for move in Move}
COL2_CODE_MOVE: dict[str, Move] = {move.attrs.col2_code: move for move in Move}

def p1_interpret_guide(ip: str) -> Iterator[Round]:
    for col1_code, col2_code in guide(ip):
        other_move = COL1_CODE_MOVE[col1_code]
        self_move = COL2_CODE_MOVE[col2_code]
        yield self_move, other_move

ROUND_SCORE: dict[Round, int] = {
    (self_move, other_move): self_move.attrs.score + outcome.attrs.score
    for (self_move, other_move), outcome in ROUND_OUTCOME.items()
}

def interpreted_guide_csv(guide: Iterable[Round]) -> str:
    return '\n'.join(','.join((
        self_move.value, other_move.value,
        ROUND_OUTCOME[self_move, other_move].value,
        str(ROUND_SCORE[self_move, other_move])
    )) for self_move, other_move in guide)

def p1_csv(ip: str) -> str:
    return interpreted_guide_csv(p1_interpret_guide(ip))

def total_score(guide: Iterable[Round]) -> int:
    return sum(ROUND_SCORE[self_move, other_move] for self_move, other_move in guide)

def p1(ip: str) -> int:
    return total_score(p1_interpret_guide(ip))

COL2_CODE_OUTCOME: dict[str, Outcome] = {outcome.attrs.col2_code: outcome for outcome in Outcome}

OTHER_MOVE_OUTCOME_SELF_MOVE: dict[tuple[Move, Outcome], Move] = {
    (other_move, outcome): self_move
    for (self_move, other_move), outcome in ROUND_OUTCOME.items()
}

def p2_interpret_guide(ip: str) -> Iterator[Round]:
    for col1_code, col2_code in guide(ip):
        other_move = COL1_CODE_MOVE[col1_code]
        outcome = COL2_CODE_OUTCOME[col2_code]
        self_move = OTHER_MOVE_OUTCOME_SELF_MOVE[other_move, outcome]
        yield self_move, other_move

def p2_csv(ip: str) -> str:
    return interpreted_guide_csv(p2_interpret_guide(ip))

def p2(ip: str) -> int:
    return total_score(p2_interpret_guide(ip))