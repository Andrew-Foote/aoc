from dataclasses import dataclass
from typing import Iterator, Self

test_inputs = [('example', '''\
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000\
''', [
    ('calories_csv', '6000,4000,11000,24000,10000'),
    ('p1', 24000),
    ('top3_calories_csv', '10000,11000,24000'),
    ('p2', 45000)
])]

@dataclass
class Elf:
    foods: list[int]

    def calories(self: Self) -> int:
        return sum(self.foods)

def elves(ip: str) -> Iterator[Elf]:
    for elf_s in ip.split('\n\n'):
        yield Elf(list(map(int, elf_s.splitlines())))

def calories(ip: str) -> Iterator[int]:
    return (elf.calories() for elf in elves(ip))

def calories_csv(ip: str) -> str:
    return ','.join(map(str, calories(ip)))

def p1(ip: str) -> int:
    return max(calories(ip))

def top3_calories(ip: str) -> list[int]:
    return sorted(calories(ip))[-3:]

def top3_calories_csv(ip: str) -> str:
    return ','.join(map(str, top3_calories(ip)))

def p2(ip: str) -> int:
    return sum(top3_calories(ip))