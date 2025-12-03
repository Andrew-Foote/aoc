from collections.abc import Iterator

test_inputs = [
    ('example', '''\
987654321111111
811111111111119
234234234234278
818181911112111''', [
        ('p1', 357)
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
