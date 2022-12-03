from typing import Iterable, Iterator

Rucksack = tuple[set[str], set[str]]

def parse_rucksacks(ip: str) -> Iterator[Rucksack]:
    for line in ip.split('\n'):
        line = line.strip()

        if line:
            halfway = len(line) // 2
            compartment1 = line[:halfway]
            compartment2 = line[halfway:]
            yield set(compartment1), set(compartment2)

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

def p1(ip: str) -> int:
    rucksacks = parse_rucksacks(ip)
    return sum(item_priority(common_item(rucksack)) for rucksack in rucksacks)

TEST_INPUT = '''\
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw\
'''

def test_p1() -> None:
    rucksacks = list(parse_rucksacks(TEST_INPUT))
    assert len(rucksacks) == 6

    assert rucksacks[0] == (set('vJrwpWtwJgWr'), set('hcsFMMfFFhFp'))
    i1 = common_item(rucksacks[0])
    assert i1 == 'p'

    assert rucksacks[1] == (set('jqHRNqRjqzjGDLGL'), set('rsFMfFZSrLrFZsSL'))
    i2 = common_item(rucksacks[1])
    assert i2 == 'L'

    assert rucksacks[2] == (set('PmmdzqPrV'), set('vPwwTWBwg'))
    i3 = common_item(rucksacks[2])
    assert i3 == 'P'

    i4 = common_item(rucksacks[3])
    assert i4 == 'v'

    i5 = common_item(rucksacks[4])
    assert i5 == 't'

    i6 = common_item(rucksacks[5])
    assert i6 == 's'

    assert item_priority(i1) == 16
    assert item_priority(i2) == 38, i2
    assert item_priority(i3) == 42
    assert item_priority(i4) == 22
    assert item_priority(i5) == 20
    assert item_priority(i6) == 19
    assert sum(map(item_priority, (i1, i2, i3, i4, i5, i6))) == 157

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

def test_p2() -> None:
    groups = list(rucksack_groups(parse_rucksacks(TEST_INPUT)))

    assert badge(groups[0]) == 'r'
    assert badge(groups[1]) == 'Z'
    assert sum(map(group_priority, groups)) == 70

def p2(ip: str) -> int:
    groups = rucksack_groups(parse_rucksacks(ip))
    return sum(map(group_priority, groups))