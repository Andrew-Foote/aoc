from collections.abc import Generator
from dataclasses import dataclass
from enum import Enum
from typing import assert_never
from solutions.python.lib import digits

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
                return a * 10 ** digits.digit_count(b) + b
            case _:
                assert_never(self)

# (I didn't end up using any of this, instead used the simpler fact that
# ADD, MUL and CAT are all increasing functions of their operands)
#
# Suppose a and b are positive integers.
#
# THEOREM 1. cmp(a + b, ab) = {
#     1 if a = 1 or b = 1,
#     0 if a = b = 2,
#     -1 if a > 1 and b > 1.
#   }.
#
#   PROOF.
#     - If a = 1, then a + b = 1 + b > b = 1 * b = ab.
#     - If b = 1, then a + b = a + 1 > a = a * 1 = ab.
#     - If a = b, then a + b <= ab is equivalent to 2a <= a^2, i.e.
#       a^2 - 2a >= 0, i.e. a(a - 2) >= 0; the quadratic polynomial on the LHS
#       has roots 0 and 2, and the sign of its leading coefficient is positive,
#       so it exceeds 0 iff a < 0 or a > 2. So given that a is a positive
#       integer, we have 2a <= a^2 when a = 2, and 2a < a^2 otherwise.
#     - If a > 1, b > 1 and a != b, then, assuming WLOG that a < b, we have
#       a + b < 2b. We also have 2b <= ab, since 2 <= a; hence a + b < ab.
#
# THEOREM 2. cmp(ab, a | b) = -1.
#
# PROOF. We can write a | b as a * 10^(n + 1) + b, where
# n = floor(log10(b)), i.e. n is the unique integer satisfying
# n <= log10(b) < n + 1. So 10^n <= b < 10^(n + 1) and hence
# ab < a * 10^(n + 1) < a * 10^(n + 1) + b = a | b.

@dataclass
class Equation:
    test_val: int
    operands: list[int]
        
    def valid_op_seqs(
        self, available_ops: list[Operator]
    ) -> Generator[tuple[Operator, ...]]:
        
        seq_len = len(self.operands) - 1

        def recurse(
            prefix: tuple[Operator, ...], result: int
        ) -> Generator[tuple[Operator, ...]]:
                                    
            if result > self.test_val:
                return

            if len(prefix) == seq_len:
                if result == self.test_val:
                    yield prefix

                return

            for op in available_ops:
                ext_prefix = prefix + (op,)
                ext_result = op.apply(result, self.operands[len(ext_prefix)])
                yield from recurse(ext_prefix, ext_result)

        yield from recurse((), self.operands[0])
        
    def has_valid_op_seq(self, available_ops: list[Operator]) -> bool:
        return next(self.valid_op_seqs(available_ops), None) is not None

def parse(ip: str) -> Generator[Equation]:
    for line in ip.splitlines():
        test_val_s, operands_s = line.split(':')
        test_val = int(test_val_s.strip())
        operands = list(map(int, operands_s.split()))
        yield Equation(test_val, operands)

P1_OPS = [Operator.ADD, Operator.MUL]

def valid_op_seqs(ip: str) -> Generator[list[tuple[Operator, ...]]]:
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

def p2_valid_op_seqs(ip: str) -> Generator[list[tuple[Operator, ...]]]:
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

# alternative approach:
# (not completely my own, i looked on the reddit and saw somebody mention 
# something about "unrolling" it in reverse from the target)
# given the target, and the list of operands
# we look at the last operand first
# we try to "divide" the target by that operand, with each operator
#  - if it's addition, we subtract the operand
#  - if it's multiplication, we divide the operand (can exit early if it's
#    not divisible
#  - if it's concatenation, we just check that it has the operand as a suffix,
#    and then take the prefix
# I'll do this in Haskell