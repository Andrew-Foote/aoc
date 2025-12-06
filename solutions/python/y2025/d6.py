from collections.abc import Iterator
from dataclasses import dataclass
from math import prod
import more_itertools as mit
from typing import Literal
from solutions.python.lib.digits import (
    int_from_digits_leading_first as fromdigits
)

test_inputs = [('example', '''\
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
''', [
    ('problems_csv', '*,123,45,6;+,328,64,98;*,51,387,215;+,64,23,314'),
    ('p1', 4277556),
    ('p2', 3263827),
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

def p2_parse(ip: str) -> Iterator[Problem]:
    lines = ip.splitlines()
    longest_line_length = max(len(line) for line in lines)

    columns: list[list[str]] = [
        [
            (line[column_index] if column_index < len(line) else ' ')
            for line in lines
        ]
        for column_index in range(longest_line_length)
    ]

    problems = mit.split_at(
        columns, lambda col: all(char == ' ' for char in col)
    )

    operand_count = len(lines) - 1

    for problem in problems:
        operation: str | None = None
        operands: list[int] = []

        for column in problem:
            last_entry = column[operand_count]

            if last_entry != ' ':
                assert operation is None
                operation = last_entry

            operand = fromdigits(
                int(d) for d in column[:operand_count] if d != ' '
            )

            operands.append(operand)
        
        assert operation is not None
        yield Problem(operation, operands)

def p2(ip: str) -> int:
    return sum(problem.answer() for problem in p2_parse(ip))
