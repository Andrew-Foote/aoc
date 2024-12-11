from collections.abc import Generator
from collections import Counter, defaultdict
import math
import numpy as np
from scipy import sparse

test_inputs = [
    ('example', '125 17', [
        ('stone_count_6', 22),
        ('p1', 55312)
    ])
]

def parse(ip: str) -> Counter[int]:
    return Counter(map(int, ip.split()))

def next_stones(stone: int) -> Generator[int]:
    if stone == 0:
        yield 1
        return
    
    digit_count = math.floor(math.log10(stone)) + 1
    q, r = divmod(digit_count, 2)

    if r:
        yield stone * 2024
        return
    
    yield from divmod(stone, 10 ** q)

def transform(stones: Counter[int]) -> Counter[int]:
    new_stones: Counter[int] = Counter()

    for stone, count in stones.items():
        for next_stone in next_stones(stone):
            new_stones[next_stone] += count

    return new_stones

def states(stones: Counter[int]) -> Generator[Counter[int]]:
    yield stones

    while True:
        stones = transform(stones)
        yield stones

def state_n(stones: Counter[int], n: int) -> Counter[int]:
    for i, stones in enumerate(states(stones)):
        if i == n:
            return stones
        
    assert False, "states(stones) is an infinite generator"

def stone_count_n(stones: Counter[int], n: int) -> int:
    return state_n(stones, n).total()

# Although the above approach works fine for both part 1 and 2, below I tried an
# alternative approach that would theoretically be extensible to higher values
# of n, based on the fact that the set of all stones with a non-zero count
# eventually stops changing, so that we can set the problem up as a linear
# recurrence where each stone in the set is a variable. However, for some
# reason this approach gives me the wrong answer. I'm not sure why.

# def stone_count_n(stones: Counter[int], n: int) -> int:
#     # empirical observation: the set of all stones with a non-zero count
#     # eventually stops changing
#     #
#     # well, more precisely, we see a state such that the set for that state
#     # equals the state for the previous state, and since the next set depends
#     # only on the previous set that means there's a cycle
#     prev_stoneset = set()
#     cum_stoneset = set()

#     for state in states(stones):
#         cur_stoneset = set(state.keys())

#         if cur_stoneset == prev_stoneset:
#             break
        
#         prev_stoneset = cur_stoneset
#         cum_stoneset |= cur_stoneset

#     stonelist = sorted(cum_stoneset)
#     prev_stones_dict: defaultdict[int, set[int]] = defaultdict(set)

#     # for each stone in the stonelist, we calculate which are its "child" stones
#     # at state n + 1, the count of stone x is going to be the sum of the counts
#     # of all the stones that have it as a child
#     # so the transition matrix will just be a row of 1s and 0s, with 1s at
#     # indices corresponding to stones that have it as a child

#     for stone in stonelist:
#         for next_stone in next_stones(stone):
#             prev_stones_dict[next_stone].add(stone)

#     stone_index_map = {stone: i for i, stone in enumerate(stonelist)}

#     data: list[int] = []
#     row: list[int] = []
#     col: list[int] = []

#     for stone, prev_stones in prev_stones_dict.items():
#         for prev_stone in prev_stones:
#             row.append(stone_index_map[stone])
#             col.append(stone_index_map[prev_stone])
#             data.append(1)

#     transition_matrix = sparse.csr_array(
# 		(data, (row, col)), shape=(len(stonelist),) * 2, dtype=np.uint64
# 	)

#     power = sparse.linalg.matrix_power(transition_matrix, n)
#     counts = power.dot(np.array([stones[stone] for stone in stonelist], dtype=np.uint64))
#     return int(np.sum(counts))

def stone_count_6(ip: str) -> int:
    return stone_count_n(parse(ip), 6)

def p1(ip: str) -> int:
    return stone_count_n(parse(ip), 25)

def p2(ip: str) -> int:
    return stone_count_n(parse(ip), 75)


