from collections.abc import Iterator
from dataclasses import dataclass
import functools as ft
import math
from typing import Self

test_inputs = [
    ('example', '''\
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47''', [
        ('correct_updates_csv','0,1,2'),
        ('middle_page_numbers_csv', '61,53,29'),
        ('p1', 143),
    ]),
]

@dataclass(frozen=True)
class Rule:
    lhs: int
    rhs: int

    @classmethod
    def parse(cls, s: str) -> Self:
        lhs, rhs = s.split('|')
        return cls(int(lhs), int(rhs))
    
@dataclass
class Update:
    nums: list[int]

    @ft.cached_property
    def indices(self) -> dict[int, int]:
        return dict((num, i) for i, num in enumerate(self.nums))

    @classmethod
    def parse(cls, s: str) -> Self:
        return cls(list(map(int, s.split(','))))

    def is_correct(self, rules: list[Rule]) -> bool:
        for rule in rules:
            ixs = self.indices
            li = ixs.get(rule.lhs, -math.inf)
            ri = ixs.get(rule.rhs, math.inf)

            if li > ri:
                return False
        
        return True

def parse(ip: str) -> tuple[list[Rule], list[Update]]:
    rules_s, updates_s = ip.split('\n\n')
    rules = list(map(Rule.parse, rules_s.splitlines()))
    updates = list(map(Update.parse, updates_s.splitlines()))
    return rules, updates    

def correct_updates(ip: str) -> Iterator[Update]:
    rules, updates = parse(ip)
    
    for update in updates:
        if update.is_correct(rules):
            yield update

def middle_page_numbers(ip: str) -> Iterator[int]:
    for update in correct_updates(ip):
        l = len(update.nums)
        assert l % 2, 'number of pages included by update is even'
        yield update.nums[l // 2]

def middle_page_numbers_csv(ip: str) -> str:
    return ','.join(map(str, middle_page_numbers(ip)))

def p1(ip: str) -> int:
    return sum(middle_page_numbers(ip))


