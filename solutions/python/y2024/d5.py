from collections import defaultdict
from collections.abc import Iterable, Iterator
from dataclasses import dataclass
import functools as ft
import itertools as it
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
        ('corrected_updates_csv', '97,75,47,61,53;61,29,13;97,75,47,29,13'),
        ('p2', 123)
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

def transitive_closure(rules: set[Rule]) -> set[Rule]:
    # actually, turns out we don't need to do this --- the supplied set of
    # rules is already transitive! (which also means the code below has not
    # been tested at all!)

    # and furthermore: as a post on reddit points out
    # (https://old.reddit.com/r/adventofcode/comments/1h7mm3w/2024_day_05_part_2_how_nice_is_the_input_a_binary/),
    # this was evident in the problem description. since we have to be able
    # to determine the middle element of each corrected update, each corrected
    # update must have a unique ordering. and if the ordering is only partial,
    # that means there are two elements (x, y) where 


    return rules

    rtl: defaultdict[int, set[int]] = defaultdict(set)

    for rule in rules:
        middle = rule.lhs
        right = rule.rhs
        lefts = rtl[right]

        for left in rtl[middle]:
            lefts.add(left)

        lefts.add(middle) 

    return {Rule(left, right) for right, lefts in rtl.items() for left in lefts}

@dataclass
class Update:
    nums: list[int]

    @ft.cached_property
    def indices(self) -> dict[int, int]:
        return dict((num, i) for i, num in enumerate(self.nums))

    @classmethod
    def parse(cls, s: str) -> Self:
        return cls(list(map(int, s.split(','))))

    def is_correct(self, rules: set[Rule]) -> bool:
        for rule in rules:
            ixs = self.indices
            li = ixs.get(rule.lhs, -math.inf)
            ri = ixs.get(rule.rhs, math.inf)

            if li > ri:
                return False
        
        return True
    
    def corrected(self, rules: set[Rule]) -> Self:
        def cmp(num1: int, num2: int) -> int:
            if num1 == num2:
                return 0
            elif Rule(num1, num2) in rules and Rule(num2, num1) in rules:
                raise ValueError('ordering is not antisymmetric')
            elif Rule(num1, num2) in rules:
                return -1
            elif Rule(num2, num1) in rules:
                return 1
            else:
                raise ValueError('ordering is not total')

        return self.__class__(sorted(self.nums, key=ft.cmp_to_key(cmp)))

def parse(ip: str) -> tuple[list[Rule], list[Update]]:
    rules_s, updates_s = ip.split('\n\n')
    rules = list(map(Rule.parse, rules_s.splitlines()))
    updates = list(map(Update.parse, updates_s.splitlines()))
    return rules, updates    

def correct_updates(ip: str) -> Iterator[Update]:
    rules, updates = parse(ip)
    
    for update in updates:
        if update.is_correct(set(rules)):
            yield update

def correct_updates_csv(ip: str) -> str:
    rules, updates = parse(ip)
    ruleset = set(rules)

    return ','.join(
        str(i) for i, update in enumerate(updates)
        if update.is_correct(ruleset)
    )

def middle_page_numbers(updates: Iterable[Update]) -> Iterator[int]:
    for update in updates:
        l = len(update.nums)
        assert l % 2, 'number of pages included by update is even'
        yield update.nums[l // 2]

def middle_page_numbers_csv(ip: str) -> str:
    return ','.join(map(str, middle_page_numbers(correct_updates(ip))))

def p1(ip: str) -> int:
    return sum(middle_page_numbers(correct_updates(ip)))

def corrected_updates(ip: str) -> Iterator[Update]:
    rules, updates = parse(ip)

    for update in updates:
        nums = set(update.nums)
        
        # part that made me struggle with this was not realizing that it was
        # important to only include the relevant rules here
        # if you compute the transitive closure of the whole set of rules
        # outside the loop and use that, you get a wrong answer for my input
        # on p2 (though not for the example input)
        relevant_rules = {
            rule for rule in rules if rule.lhs in nums and rule.rhs in nums
        }

        closure = transitive_closure(relevant_rules)

        if not update.is_correct(relevant_rules):
            yield update.corrected(closure)

def p2(ip: str) -> int:
    return sum(middle_page_numbers(corrected_updates(ip)))
