from collections import Counter
from dataclasses import dataclass
from enum import Enum
import functools as ft
import itertools as it
from typing import assert_never, cast, Iterator, Literal, Self

DEBUG = False

test_inputs = [
    (
        'no_unknown_example', '''\
#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1''', [
        ],
    ),
    (
        'example', '''\
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
''', [
            ('arrangement_counts_csv', '1,4,1,1,4,10'),
            ('p1', 21),
            ('p2_arrangement_counts_csv', '1,16384,1,16,2500,506250'),
            ('p2', 525152),
        ]
    ),
]

class SpringState(Enum):
    OPERATIONAL = '.'
    DAMAGED = '#'
    UNKNOWN = '?'

    def __str__(self) -> str:
        return self.value

    @classmethod
    def parse(cls, c: str) -> Self:
        return cls(c)

KnownSpringState = (
    Literal[SpringState.OPERATIONAL] | Literal[SpringState.DAMAGED]
)

KNOWN_SPRING_STATES: tuple[KnownSpringState, KnownSpringState] = (
    SpringState.OPERATIONAL, SpringState.DAMAGED
)

@dataclass(frozen=True)
class Row[T: SpringState]:
    items: tuple[T, ...]

    def __str__(self) -> str:
        return ''.join(map(str, self.items))

    @classmethod
    def parse(cls, s: str) -> Self:
        return cls(tuple(cast(T, SpringState.parse(c)) for c in s))

UnknownRow = Row[SpringState]
KnownRow = Row[KnownSpringState]

def parse(ip: str) -> Iterator[tuple[UnknownRow, tuple[int, ...]]]:
    lines = ip.splitlines()

    for line in lines:
        row_s, sizes_s = line.split()
        row: UnknownRow = Row.parse(row_s)
        sizes = tuple(map(int, sizes_s.split(',')))
        yield row, sizes

# This clever NFA-based solution was adapted from clrfl's code at the following
# URL: https://github.com/clrfl/AdventOfCode2023/tree/master/12
def arrangement_count(row: UnknownRow, sizes: tuple[int, ...]) -> int:
    nfa_state_types = (
        '.' + '.'.join('#' * (size - 1) + '@' for size in sizes) + ','
    )

    nfa_states = set(range(len(nfa_state_types)))
    nfa_inputs = set(SpringState)

    tr: dict[tuple[int, SpringState], set[int]] = {}

    for state in nfa_states:
        state_type = nfa_state_types[state]

        if state_type == '.':
            tr[state, SpringState.OPERATIONAL] = {state}
            tr[state, SpringState.DAMAGED] = {state + 1}
            tr[state, SpringState.UNKNOWN] = {state, state + 1}
        elif state_type == '#':
            tr[state, SpringState.OPERATIONAL] = set()
            tr[state, SpringState.DAMAGED] = {state + 1}
            tr[state, SpringState.UNKNOWN] = {state + 1}
        elif state_type == '@':
            tr[state, SpringState.OPERATIONAL] = {state + 1}
            tr[state, SpringState.DAMAGED] = set()
            tr[state, SpringState.UNKNOWN] = {state + 1}
        elif state_type == ',':
            tr[state, SpringState.OPERATIONAL] = {state}
            tr[state, SpringState.DAMAGED] = set()
            tr[state, SpringState.UNKNOWN] = {state}

    start_state = 0
    end_states = {len(nfa_states) - 2, len(nfa_states) - 1}

    states = Counter((start_state,))
    next_states: Counter[int] = Counter()

    # The NFA consumes the row.
    # Instead of just keeping track of the current states, we
    # keep track of a "visitor count" for each current state.
    # After consuming the character, the visitor count for each
    # of the newly entered states will be the sum of the visitor
    # counts of all the states it transitioned from.
    # (The visitor count is "duplicated" whenever a node can
    # transition to more than one state.)
    # The combined number of visitors of the end states, after
    # the whole row is consumed, can be taken as the number of
    # different "paths" we can take to reach an end state.

    for row_state in row.items:
        for state, visitor_count in states.items():
            for next_state in tr[state, row_state]:
                next_states[next_state] += visitor_count

        states = next_states
        next_states = Counter()

    return sum(states[state] for state in end_states)


    # keys of this dictionary are all the states the NFA is currently in
    sdict: dict[int, int] = {0: 1}
    counter: Counter[int] = Counter()

    for state in row.items:
        print(state, sdict)

        for s in sdict:
            if state == SpringState.UNKNOWN:
                if s + 1 < len(nfa):
                    counter[s + 1] += sdict[s]

                if nfa[s] == '.':
                    counter[s] += sdict[s]

            elif state == SpringState.OPERATIONAL:
                if s + 1 < len(nfa) and nfa[s + 1] == '.':
                    counter[s + 1] += sdict[s]

                if nfa[s] == '.':
                    counter[s] += sdict[s]

            elif state == SpringState.DAMAGED:
                if s + 1 < len(nfa) and nfa[s + 1] == '#':
                    counter[s + 1] += sdict[s]

        sdict = dict(counter)
        counter = Counter()

    endstates = len(nfa) - 1, len(nfa) - 2
    return sdict[len(nfa) - 1] + sdict[len(nfa) - 2]


def p2(ip: str) -> int:
    res = 0

    for row, sizes in parse(ip):
        row = unfold_row(row)
        sizes = unfold_sizes(sizes)
        res += fuck(row, sizes)

    return res

def arrangement_counts(ip: str) -> Iterator[int]:
    for row, sizes in parse(ip):
        yield arrangement_count(row, sizes)

def arrangement_counts_csv(ip: str) -> str:
    return ','.join(map(str, arrangement_counts(ip)))

def p1(ip: str) -> int:
    return sum(arrangement_counts(ip))

def unfold_row(row: UnknownRow) -> UnknownRow:
    copies = [row.items for _ in range(5)]
    
    return Row(sum(
        tuple((*copy, SpringState.UNKNOWN) for copy in copies), start=()
    )[:-1])

def unfold_sizes(sizes: tuple[int, ...]) -> tuple[int, ...]:
    return sum((sizes for _ in range(5)), start=())

def p2_arrangement_counts(ip: str) -> Iterator[int]:
    for row, sizes in parse(ip):
        row = unfold_row(row)
        sizes = unfold_sizes(sizes)
        yield arrangement_count(row, sizes)

def p2_arrangement_counts_csv(ip: str) -> str:
    return ','.join(map(str, p2_arrangement_counts(ip)))

def p2(ip: str) -> int:
    return sum(p2_arrangement_counts(ip))
