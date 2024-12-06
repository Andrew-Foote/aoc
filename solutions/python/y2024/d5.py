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
    domain = set().union(*({rule.lhs, rule.rhs} for rule in rules))

    while True:
        complete = True

        for a, b, c in it.product(domain, repeat=3):
            r1 = Rule(a, b)
            r2 = Rule(b, c)
            r3 = Rule(a, c)

            if r1 in rules and r2 in rules and r3 not in rules:
                complete = False
                rules.add(r3)

        if complete:
            break

    return rules

# copying the algorithm from wikipedia
# although it turns out my transitive closure algorithm is fast enough
# def toposort(adjacency_list: dict[int, int]) -> list[int]:
#     result: list[int] = []
#     not_permmarked: set[int] = set(adjacency_list.keys())
#     tempmarked: set[int] = set()
    
#     def visit(num: int) -> None:
#         if num not in not_permmarked:
#             return
        
#         if num in tempmarked:
#             raise ValueError('cycle')
        
#         for num2 in adjacency_list[num]:
#             visit(num2)

#         not_permmarked.remove(num)

#         # at this point, all nums that succeed num
#         # should already be present in the list
#         result.preppend(num)

#     while not_permmarked:
#         num = not_permmarked.pop()
#         visit(num)

#     return result

# def transitive_closure(ruleset: set[Rule]) -> set[Rule]:
#     rtl: defaultdict[int, set[int]] = defaultdict(set)

#     for rule in ruleset:
#         middle = rule.lhs
#         right = rule.rhs
#         lefts = rtl[right]

#         for left in rtl[middle]:
#             lefts.add(left)

#         lefts.add(middle) 

#     return {Rule(left, right) for right, lefts in rtl.items() for left in lefts}

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
    
    def corrected(self, rules: set[Rule]) -> Self:
        def cmp(num1: int, num2: int) -> int:
            if num1 == num2:
                return 0
            elif Rule(num1, num2) in rules and Rule(num2, num1) in rules:
                print(rules)
                print(num1, num2)
                #raise ValueError('ordering is not antisymmetric')
                return 0
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
        if update.is_correct(rules):
            yield update

def correct_updates_csv(ip: str) -> str:
    rules, updates = parse(ip)
    
    return ','.join(
        str(i) for i, update in enumerate(updates)
        if update.is_correct(rules)
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
        
        relevant_rules = {
            rule for rule in rules if rule.lhs in nums and rule.rhs in nums
        }

        closure = transitive_closure(relevant_rules)

        if not update.is_correct(relevant_rules):
            yield update.corrected(closure)

def p2(ip: str) -> int:
    return sum(middle_page_numbers(corrected_updates(ip)))
# 5326 is too high