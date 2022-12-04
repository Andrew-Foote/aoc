from dataclasses import dataclass
import functools as ft
from typing import Iterator

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
    ('p1', '24000'),
    ('top3_calories_csv', '10000,11000,24000'),
    ('p2', '45000')
])]

@dataclass
class Elf:
    foods: list[int]

    @ft.cached_property
    def calories(self):
        return sum(self.foods)

def elves(ip: str) -> Iterator[Elf]:
    elf_strings = filter(None, (s.strip() for s in ip.split('\n\n')))

    for elf_string in elf_strings:
        food_strings = filter(None, (s.strip() for s in elf_string.split('\n')))
        yield Elf(list(map(int, food_strings)))

def calories(ip: str) -> Iterator[int]:
    return (elf.calories for elf in elves(ip))

def calories_csv(ip: str) -> str:
    return ','.join(map(str, calories(ip)))

def p1(ip: str) -> str:
    return str(max(calories(ip)))

def top3_calories(ip: str) -> str:
    return sorted(calories(ip))[-3:]

def top3_calories_csv(ip: str) -> str:
    return ','.join(map(str, top3_calories(ip)))

def p2(ip: str) -> str:
    return str(sum(top3_calories(ip)))