import itertools as it
from typing import Iterable, Iterator
from utils import joinlines

test_inputs = [('example', '''\
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw\
''', [
    ('first3_compartments_csv', joinlines([
        'vJrwpWtwJgWr,hcsFMMfFFhFp',
        'jqHRNqRjqzjGDLGL,rsFMfFZSrLrFZsSL',
        'PmmdzqPrV,vPwwTWBwg'
    ])),
    ('p1_csv', joinlines(['p,16', 'L,38', 'P,42', 'v,22', 't,20', 's,19'])),
    ('p1', '157'),
    ('badges_csv', 'r,Z'),
    ('p2', '70')
])]

def compartments(ip: str) -> Iterator[tuple[str, str]]:
    for line in ip.splitlines():
        halfway = len(line) // 2
        yield line[:halfway], line[halfway:]

def first3_compartments_csv(ip: str) -> list[Iterator]:
    return joinlines(f'{c1},{c2}' for c1, c2 in it.islice(compartments(ip), 3))

Rucksack = tuple[set[str], set[str]]

def rucksacks(ip: str) -> Iterator[Rucksack]:
    for c1, c2 in compartments(ip):
        yield set(c1), set(c2)

def common_item(rucksack: Rucksack) -> list[int]:
    isect = rucksack[0] & rucksack[1]
    assert len(isect) == 1
    return isect.pop()

def item_priority(item: str) -> int:
    if ord(item) >= ord('a'):
        return ord(item) - ord('a') + 1
    elif ord(item) >= ord('A'):
        return ord(item) - ord('A') + 27
    assert False

def p1_csv(ip: str) -> str:
    csv = []

    for rucksack in rucksacks(ip):
        item = common_item(rucksack)
        csv.append(f'{item},{item_priority(item)}')

    return joinlines(csv)

def p1(ip: str) -> int:
    return sum(item_priority(common_item(rucksack)) for rucksack in rucksacks(ip))

RucksackGroup = tuple[Rucksack, Rucksack, Rucksack]

def rucksack_groups(rucksacks: Iterable[Rucksack]) -> Iterator[RucksackGroup]:
    current = []

    for rucksack in rucksacks:
        current.append(rucksack)

        if len(current) == 3:
            yield tuple(current)
            current.clear()

def rucksack_all_items(rucksack: Rucksack) -> set[str]:
    return rucksack[0] | rucksack[1]

def badge(rucksack_group: RucksackGroup) -> str:
    g1, g2, g3 = rucksack_group

    isect = (
        rucksack_all_items(g1)
        & rucksack_all_items(g2)
        & rucksack_all_items(g3)
    )

    assert len(isect) == 1
    return isect.pop()

def group_priority(rucksack_group: RucksackGroup) -> int:
    return item_priority(badge(rucksack_group))

def badges_csv(ip) -> str:
    return ','.join(badge(group) for group in rucksack_groups(rucksacks(ip)))

def p2(ip: str) -> int:
    groups = rucksack_groups(rucksacks(ip))
    return sum(map(group_priority, groups))