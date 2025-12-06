from collections.abc import Iterator
from dataclasses import dataclass
from math import prod
from typing import Literal

test_inputs = [('example', '''\
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
''', [
    ('problems_csv', '*,123,45,6;+,328,64,98;*,51,387,215;+,64,23,314'),
    ('p1', 4277556),
])]

@dataclass(frozen=True, slots=True)
class Problem:
    operation: str
    operands: list[int]

    def answer(self) -> int:
        match self.operation:
            case '+':
                return sum(self.operands)
            case '*':
                return prod(self.operands)
            case _:
                assert False

def parse(ip: str) -> Iterator[Problem]:
    lines = ip.splitlines()
    operations = lines[-1].split()
    operand_lists = [line.split() for line in lines[:-1]]
    
    for operation, *operands in zip(operations, *operand_lists):
        yield Problem(operation, [int(operand) for operand in operands])

def problems_csv(ip: str) -> str:
    problem_strings = []

    for problem in parse(ip):
        problem_strings.append(','.join((problem.operation, *map(str, problem.operands))))

    return ';'.join(problem_strings)

def p1(ip: str) -> int:
    return sum(problem.answer() for problem in parse(ip))

