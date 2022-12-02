from dataclasses import dataclass
import functools as ft
from typing import Iterator

@dataclass
class Elf:
    foods: list[int]

    @ft.cached_property
    def calories(self):
        return sum(self.foods)

def parse_elves(ip: str) -> Iterator[Elf]:
    elf_strings = filter(None, (s.strip() for s in ip.split('\n\n')))

    for elf_string in elf_strings:
        food_strings = filter(None, (s.strip() for s in elf_string.split('\n')))
        yield Elf(list(map(int, food_strings)))

TEST_INPUT = '''\
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
'''

def test() -> None:
    elves = list(parse_elves(TEST_INPUT))
    assert len(elves) == 5
    assert elves[0].calories == 6000
    assert elves[1].calories == 4000
    assert elves[2].calories == 11000
    assert elves[3].calories == 24000
    assert elves[4].calories == 10000
    assert max(range(5), key=lambda i: elves[i].calories) == 3
    elves.sort(key=lambda elf: elf.calories)
    assert elves[-1].calories == 24000
    assert elves[-2].calories == 11000
    assert elves[-3].calories == 10000
    assert sum(elf.calories for elf in elves[-3:]) == 45000

def p1(ip: str) -> int:
    elves = parse_elves(ip)
    return max(elves, key=lambda elf: elf.calories).calories

def p2(ip: str) -> int:
    elves = parse_elves(ip)
    return sum(elf.calories for elf in sorted(elves, key=lambda elf: elf.calories)[-3:])