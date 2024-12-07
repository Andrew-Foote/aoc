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
        ('valid_op_seqs_csv', '*#+,*;*,+#######+,*,+'),
        ('p1', 3749),
        ('p2_valid_op_seqs_csv', '*#+,*;*,+##|#*,|,*##|,+##+,*,+'),
        ('p2', 11387)
    ]),
]

class Operator(Enum):
    ADD = 0
    MUL = 1
    CAT = 2

    def __str__(self) -> str:
        match self:
            case Operator.ADD:
                return '+'
            case Operator.MUL:
                return '*'
            case Operator.CAT:
                return '|'
            case _:
                assert_never(self)

    def apply(self, a: int, b: int) -> int:
        match self:
            case Operator.ADD:
                return a + b
            case Operator.MUL:
                return a * b
            case Operator.CAT:
                return int(str(a) + str(b))
            case _:
                assert_never(self)

@dataclass
class Equation:
    test_val: int
    operands: list[int]
    
    def poss_op_seqs(
        self, available_ops: list[Operator]
    ) -> Iterator[tuple[Operator, ...]]:
    
        return it.product(available_ops, repeat=len(self.operands) - 1)

    def op_seq_result(self, op_seq: tuple[Operator, ...]) -> int:
        assert len(op_seq) == len(self.operands) - 1
        result = self.operands[0]

        for operand, operator in zip(self.operands[1:], op_seq):
            result = operator.apply(result, operand)

        return result

    def op_seq_is_valid(self, op_seq: tuple[Operator, ...]) -> bool:
        return self.op_seq_result(op_seq) == self.test_val

    def valid_op_seqs(
        self, available_ops: list[Operator]
    ) -> Iterator[tuple[Operator, ...]]:
    
        for op_seq in self.poss_op_seqs(available_ops):
            if self.op_seq_is_valid(op_seq):
                yield op_seq

    def has_valid_op_seq(self, available_ops: list[Operator]) -> bool:
        return next(self.valid_op_seqs(available_ops), None) is not None

def parse(ip: str) -> Iterator[Equation]:
    for line in ip.splitlines():
        test_val_s, operands_s = line.split(':')
        test_val = int(test_val_s.strip())
        operands = list(map(int, operands_s.split()))
        yield Equation(test_val, operands)

P1_OPS = [Operator.ADD, Operator.MUL]

def valid_op_seqs(ip: str) -> Iterator[list[tuple[Operator, ...]]]:
    for equation in parse(ip):
        yield list(equation.valid_op_seqs(P1_OPS))

def valid_op_seqs_csv(ip: str) -> str:
    return '#'.join(
        ';'.join(','.join(map(str, op_seq)) for op_seq in op_seq_set)
        for op_seq_set in valid_op_seqs(ip)
    )

def p1(ip: str) -> int:
    return sum(
        equation.test_val for equation in parse(ip)
        if equation.has_valid_op_seq(P1_OPS)
    )

P2_OPS = [Operator.ADD, Operator.MUL, Operator.CAT]

def p2_valid_op_seqs(ip: str) -> Iterator[list[tuple[Operator, ...]]]:
    for equation in parse(ip):
        yield list(equation.valid_op_seqs(P2_OPS))

def p2_valid_op_seqs_csv(ip: str) -> str:
    return '#'.join(
        ';'.join(','.join(map(str, op_seq)) for op_seq in op_seq_set)
        for op_seq_set in p2_valid_op_seqs(ip)
    )

def p2(ip: str) -> int:
    return sum(
        equation.test_val for equation in parse(ip)
        if equation.has_valid_op_seq(P2_OPS)
    )