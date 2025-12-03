from collections.abc import Iterator
from solutions.python.lib.digits import int_from_digits_leading_first as fromdigits
import itertools as it

test_inputs = [
    ('example', '''\
987654321111111
811111111111119
234234234234278
818181911112111''', [
        ('p1', 357),
        ('p2_largest_joltages_csv', '987654321111,811111111119,434234234278,888911112111'),
        ('p2', 3121910778619),
    ])
]

def parse(ip: str) -> Iterator[list[int]]:
    for line in ip.splitlines():
        yield [int(d) for d in line]

def possible_joltages(bank: list[int]) -> Iterator[int]:
    for i in range(len(bank)):
        for j in range(i + 1, len(bank)):
            d1 = bank[i]
            d0 = bank[j]
            yield d1 * 10 + d0

def largest_joltage(bank: list[int]) -> int:
    return max(possible_joltages(bank))

def largest_joltages(ip: str) -> Iterator[int]:
    for bank in parse(ip):
        yield largest_joltage(bank)

def p1(ip: str) -> int:
    return sum(largest_joltages(ip))

def possible_index_lists(length: int, n: int, cur: list[int]) -> Iterator[list[int]]:
    if len(cur) == n:
        yield cur
        return

    start = cur[-1] + 1 if cur else 0

    for i in range(start, length):
        yield from possible_index_lists(length, n, cur + [i])

def possible_joltages_p2(bank: list[int], n: int=12) -> Iterator[int]:
    for indices in possible_index_lists(len(bank), n, []):
        ds = [bank[i] for i in indices]
        yield fromdigits(ds)

def largest_joltage_p2(bank: list[int]) -> int:
    return max(possible_joltages_p2(bank))

def largest_joltages_p2(ip: str) -> Iterator[int]:
    for bank in parse(ip):
        print(f'checking bank {bank}')
        yield largest_joltage_p2(bank)

def p2_largest_joltages_csv(ip: str) -> str:
    return ','.join(map(str, largest_joltages_p2(ip)))

def p2(ip: str) -> int:
    return sum(largest_joltages_p2(ip))
