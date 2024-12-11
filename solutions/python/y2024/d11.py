from collections.abc import Generator
from collections import Counter
import math

test_inputs = [
    ('example', '125 17', [
        ('stone_count_6', 22),
        ('p1', 55312)
    ])
]

def parse(ip: str) -> Counter[int]:
    return Counter(map(int, ip.split()))

def transform(stones: Counter[int]) -> Counter[int]:
    new_stones: Counter[int] = Counter()

    for stone, count in stones.items():
        if stone == 0:
            new_stones[1] += count
            continue

        digit_count = math.floor(math.log10(stone)) + 1
        q, r = divmod(digit_count, 2)

        if r:
            new_stones[stone * 2024] += count
        else:
            left, right = divmod(stone, 10 ** q)
            new_stones[left] += count
            new_stones[right] += count

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

def stone_count_6(ip: str) -> int:
    return stone_count_n(parse(ip), 6)

def p1(ip: str) -> int:
    return stone_count_n(parse(ip), 25)

def p2(ip: str) -> int:
    return stone_count_n(parse(ip), 75)
