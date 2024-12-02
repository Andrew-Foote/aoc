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
            ('arrangement_counts_naive_csv', '1,4,1,1,4,10'),
            ('arrangement_counts_csv', '1,4,1,1,4,10'),
            ('p1_naive', 21),
            ('p1', 21),
            ('p2_arrangement_counts_csv', '1,16384,1,16,2500,506250'),
            ('p2', 525152),
        ]
    ),
]
'''
?###???????? ? ?###???????? ? ?###???????? ? ?###???????? ? ?###????????
?###????????   ?###????????   ?###????????   ?###????????   ?###????????

'''


# ???.### 1,1,3
# .??..??...?##. 1,1,3
# ?#?#?#?#?#?#?#? 1,3,1,6
# ????.#...#... 4,1,1
# ????.######..#####. 1,6,5
# ?###???????? 3,2,1

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

def poss_repls(row: UnknownRow) -> Iterator[KnownRow]:
    """Yields all rows obtainable by replacing all the unknown spring states
    within the original row with known spring states."""

    unknowns = [
        i for i, state in enumerate(row.items)
        if state == SpringState.UNKNOWN
    ]

    for irepls in it.product(KNOWN_SPRING_STATES, repeat=len(unknowns)):
        repl = list(row.items)

        for i, irepl in zip(unknowns, irepls):
            repl[i] = irepl

        yield Row(cast(tuple[KnownSpringState, ...], tuple(repl)))

def get_sizes(row: KnownRow) -> tuple[int, ...]:
    result: list[int] = []
    counter: int = 0

    for state in row.items:
        match state:
            case SpringState.OPERATIONAL:
                if counter:
                    result.append(counter)
                    counter = 0
            case SpringState.DAMAGED:
                counter += 1
            case _:
                assert_never(state)

    if counter:
        result.append(counter)

    return tuple(result)

def poss_repl_is_compatible(row: KnownRow, sizes: tuple[int, ...]) -> bool:
    return get_sizes(row) == sizes

def compatible_repls(
    row: UnknownRow, sizes: tuple[int, ...]
) -> Iterator[KnownRow]:

    for repl in poss_repls(row):
        # print(f'  poss repl: {repl}; {get_sizes(repl)}', end=' ')

        if poss_repl_is_compatible(repl, sizes):
            yield repl
            # print('compatible')
        else:
            # print('not compatible')
            pass

def arrangement_counts_naive(ip: str) -> tuple[int, ...]:
    result = []

    for row, sizes in parse(ip):
        # print(f'\nrow: {row}; sizes: {sizes}')
        result.append(sum(True for repl in compatible_repls(row, sizes)))

    return tuple(result)

def arrangement_counts_naive_csv(ip: str) -> str:
    counts = arrangement_counts_naive(ip)
    return ','.join(map(str, counts))

def p1_naive(ip: str) -> int:
    # this is already pretty slow
    return sum(arrangement_counts_naive(ip))

def unfold_row(row: UnknownRow) -> UnknownRow:
    copies = [row.items for _ in range(5)]
    
    return Row(sum(
        tuple((*copy, SpringState.UNKNOWN) for copy in copies), start=()
    )[:-1])

def unfold_sizes(sizes: tuple[int, ...]) -> tuple[int, ...]:
    return sum((sizes for _ in range(5)), start=())

# To do part 2, we need to take an alternative approach: instead of looking at
# the row and considering all possible replacements, we look at the sizes and
# consider all possible ways of placing the contiguous groups.
#
# Given a group, what are its possible locations?
#
# Without considering placement of other groups, they are all those spans
# within the row that don't contain any operational springs (only damaged or
# unknown), and also aren't adjacent to any damaged springs (only operational
# or unknown). The latter condition is because these are the maximal contiguous
# groups.
#
# If we're also considering the other groups, they'll be placed in order; the
# new group will have to be placed at an offset which is at least 1 greater
# than the end index of the group last placed.
#
# Moreover, if it's the last group to be placed, it has to cover all the
# remaining damaged springs that aren't already covered by a group; otherwise
# the whole group-set can be disregarded.

def poss_group_locs(
    row: UnknownRow, size: int, offset: int
) -> tuple[int, ...]:
    """Given a row, a size, and an offset, returns all possible start indices
    for a contiguous group of damaged springs, where the start index is greater
    than the offset."""

    result: list[int] = []

    if DEBUG: print(f'     poss_group_locs({row}, {size}, {offset})')

    rowlen = len(row.items)

    for i in range(offset, rowlen - size + 1):
        span = range(i, i + size)

        # ensure span is not adjacent to a known broken spring
        pre_span = i - 1
        post_span = i + size

        if (
            (pre_span >= 0 and row.items[pre_span] == SpringState.DAMAGED)
            or (
                post_span < rowlen
                and row.items[post_span] == SpringState.DAMAGED
            )
        ):
            # print(f'      {i} cannot be group loc cos dmg adj')
            continue

        # ensure no known operational springs within span

        if any(row.items[j] == SpringState.OPERATIONAL for j in span):
            # print(f'      {i} cannot be group loc cos op within span')
            continue

        # maybe we keep track of the uncovered_brokens as one of the parameters
        # and when we're adding the last group, we require that it includes all
        # the uncovered_brokens

        result.append(i)

    if DEBUG: print(f'    -> {result}')
    return tuple(result)

def group_locs_valid(
    row: UnknownRow, locs: tuple[int, ...], sizes: tuple[int, ...]
) -> bool:
    """
    >>> group_locs_valid(Row.parse('#.'), (1,), (1,))
    False
    """

    damaged: set[int] = set()

    for loc, size in zip(locs, sizes):
        for i in range(loc, loc + size):
            damaged.add(i)

    for i, state in enumerate(row.items):
        if i not in damaged and state == SpringState.DAMAGED:
            return False

    return True

def repl_from_group_locs(
    row: UnknownRow, locs: tuple[int, ...], sizes: tuple[int, ...]
) -> KnownRow:

    damaged: set[int] = set()

    for loc, size in zip(locs, sizes):
        for i in range(loc, loc + size):
            damaged.add(i)

    repl: list[KnownSpringState] = []

    for i, state in enumerate(row.items):
        if i in damaged:
            assert state in (SpringState.DAMAGED, SpringState.UNKNOWN)
            repl.append(SpringState.DAMAGED)
        else:
            repl.append(SpringState.OPERATIONAL)

    return Row(tuple(repl))

@ft.cache
def pgls(row: UnknownRow, sizes: tuple[int, ...]) -> list[tuple[int, ...]]:
    if not sizes:
        return [()]

    result = []
    size = sizes[0]
    sizes = sizes[1:]

    required_space = sum(sizes) + len(sizes) - 1

    for loc in poss_group_locs(row, size, 0):
        offset = loc + size + 1
        new_row_len = len(row.items) - offset

        if new_row_len < required_space:
            continue

        new_row = Row(row.items[offset:])

        for remaining_locs in poss_group_loc_sets(new_row, sizes):
            result.append((loc, *(offset + l for l in remaining_locs)))

    return result

def poss_group_loc_sets(
    row: UnknownRow, sizes: tuple[int, ...], offset: int=0
) -> list[tuple[int, ...]]:

    if DEBUG: print(f'     poss_group_loc_sets({row}, {sizes}, {offset})')
    assert offset == 0
    return pgls(row, sizes)

    if not sizes:
        return [()]

    result = []

    size = sizes[0]
    sizes = sizes[1:]

    for loc in poss_group_locs(row, size, offset):
        new_offset = loc + size + 1

        for remaining_locs in poss_group_loc_sets(row, sizes, new_offset):
            result.append((loc, *remaining_locs))

    return result

def valid_group_loc_sets(
    row: UnknownRow, sizes: tuple[int, ...], offset: int=0
) -> Iterator[tuple[int, ...]]:

    loc_sets = poss_group_loc_sets(row, sizes, offset)
    print('got loc sets')

    validcount = 0
    totcount = 0
    
    for locs in loc_sets:
        print(f'consdiring locs: {locs}')
        # repl = repl_from_group_locs(row, locs, sizes)
        #if DEBUG: print(f'  {locs}; {repl}')
            
        # if not poss_repl_is_compatible(repl, sizes):
        #     print(repl)
        #     pass
        totcount += 1

        # i think it's mostly just slowness in the validity
        # checking for each loc set
        if group_locs_valid(row, locs, sizes):
            if DEBUG: print('    valid')
            validcount += 1

            # for i, (rowstate, replstate) in enumerate(zip(row.items, repl.items)):
            #   if replstate == SpringState.OPERATIONAL and rowstate == SpringState.DAMAGED:
            #       if DEBUG: print(row, sizes, offset, locs)
            #       if DEBUG: print(repl)
            #       if DEBUG: print(i, rowstate, replstate)

            yield locs
        else:
            #print('botvakdu')
            if DEBUG: print('    not valid')
            pass

    print(f'{validcount=}, {totcount=}, {validcount/totcount}')

def arrangement_counts(ip: str) -> Iterator[int]:
    for row, sizes in parse(ip):
        if DEBUG: print(f'\nrow: {row}; sizes: {sizes}')
        yield sum(True for _ in valid_group_loc_sets(row, sizes))

def arrangement_counts_csv(ip: str) -> str:
    return ','.join(map(str, arrangement_counts(ip)))

def p1(ip: str) -> int:
    return sum(arrangement_counts(ip))

def p2_arrangement_counts_naive(ip: str) -> Iterator[int]:
    for row, sizes in parse(ip):
        row = unfold_row(row)
        sizes = unfold_sizes(sizes)
        yield sum(1 for repl in compatible_repls(row, sizes))

def p2_arrangement_counts_naive_csv(ip: str) -> str:
    return ','.join(map(str, p2_arrangement_counts_naive(ip)))

def p2_arrangement_counts(ip: str) -> Iterator[int]:
    for row, sizes in parse(ip):
        print(row)
        row = unfold_row(row)
        sizes = unfold_sizes(sizes)
        yield sum(1 for _ in valid_group_loc_sets(row, sizes))

def p2_arrangement_counts_csv(ip: str) -> str:
    return ','.join(map(str, p2_arrangement_counts(ip)))

def p2(ip: str) -> int:
    return sum(p2_arrangement_counts(ip))
