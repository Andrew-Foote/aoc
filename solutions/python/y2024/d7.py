from collections.abc import Iterator
from dataclasses import dataclass
from enum import Enum
import itertools as it
from typing import assert_never

test_inputs = [
    ('example', '''\
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
''', [
        ('valid_op_seqs_csv', '*|+,*;*,+|||||||+,*,+'),
        ('p1', 3749),
        ('p2', 0)
    ]),
]


class Operator(Enum):
    ADD = 0
    MUL = 1

    def __str__(self) -> str:
        match self:
            case Operator.ADD:
                return '+'
            case Operator.MUL:
                return '*'
            case _:
                assert_never(self)

    def apply(self, a: int, b: int) -> int:
        match self:
            case Operator.ADD:
                return a + b
            case Operator.MUL:
                return a * b
            case _:
                assert_never(self)

@dataclass
class Equation:
    test_val: int
    operands: list[int]

    def op_seq_result(self, op_seq: tuple[Operator, ...]) -> int:
        assert len(op_seq) == len(self.operands) - 1
        result = self.operands[0]

        for operand, operator in zip(self.operands[1:], op_seq):
            result = operator.apply(result, operand)

        return result

    def op_seq_is_valid(self, op_seq: tuple[Operator, ...]) -> bool:
        return self.op_seq_result(op_seq) == self.test_val

    def valid_op_seqs(self) -> set[tuple[Operator, ...]]:
        poss_op_seqs = it.product(Operator, repeat=len(self.operands) - 1)
        result: set[tuple[Operator, ...]] = set()

        for op_seq in poss_op_seqs:
            if self.op_seq_is_valid(op_seq):
                result.add(op_seq)

        return result


def parse(ip: str) -> Iterator[Equation]:
    for line in ip.splitlines():
        test_val_s, operands_s = line.split(':')
        test_val = int(test_val_s.strip())
        operands = list(map(int, operands_s.split()))
        yield Equation(test_val, operands)

def valid_op_seqs(ip: str) -> Iterator[list[tuple[Operator, ...]]]:
    for equation in parse(ip):
        yield equation.valid_op_seqs()

def valid_op_seqs_csv(ip: str) -> str:
    return '|'.join(
        ';'.join(','.join(map(str, op_seq)) for op_seq in op_seq_set)
        for op_seq_set in valid_op_seqs(ip)
    )

def p1(ip: str) -> int:
    return sum(
        equation.test_val for equation in parse(ip)
        if equation.valid_op_seqs()
    )