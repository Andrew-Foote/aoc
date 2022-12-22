from abc import ABC, abstractmethod
import itertools as it
import functools as ft
from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from typing import Self
import re
import operator
import sympy

test_inputs = [('example', '''\
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32\
''', [
    ('p1', '152'),
    ('p2', '301')
])]

class Expr(ABC):
    @abstractmethod
    def eval(self: Self, env: dict['Var', Self]) -> Self:
        ...

    @abstractmethod
    def immsubexprs(self: Self) -> Iterator[Self]:
        ...

    @abstractmethod
    def solve(self: Self, x: 'Var') -> dict['Var', Expr]:
        ...

    def __add__(self: Self, other: Self) -> Self:
        return Sum(self, other)

    def __sub__(self: Self, other: Self) -> Self:
        return Difference(self, other)

    def __mul__(self: Self, other: Self) -> Self:
        return Product(self, other)

    def __truediv__(self: Self, other: Self) -> Self:
        return Quotient(self, other)

@dataclass
class Var(Expr):
    name: str

    def eval(self, env):
        if self not in env:
            return self

        return env[self].eval(env)

    def immsubexprs(self):
        yield from ()

    def coeffs(self):
        return {(self,): self}

@dataclass
class Const(Expr):
    value: int

    def eval(self, env):
        return self

    def immsubexprs(self):
        yield from ()

    def coeffs(self):
        return {(): self}

@dataclass
class Sum(Expr):
    terms: list[Expr]

    def eval(self, env):
        return sum(term.eval(env) for term in self.terms)

    def immsubexprs(self):
        yield from self.terms

    def coeffs(self):
        coeffs_per_term = [term.coeffs() for term in self.terms]
        var_exprs = set().union(*(coeffs.keys() for coeffs in coeffs_per_term))
        
        return {
            sum(coeffs.get(var_expr, 0) for coeffs in coeffs_per_term)
            for var_expr in var_exprs
        }

@dataclass
class Difference(Expr):
    minuend: Expr
    subtrahend: Expr

    def eval(self, env):
        return self.minuend.eval(env) - eval(self.subtrahend, env)

    def immsubexprs(self):
        yield self.minuend
        yield self.subtrahend

    def solve(self, x, a):
        if x not in self.minuend:
            # a - f(x) = b ==> a + b = f(x)
            return 


        return cancelled.

@dataclass
class Product(Expr):
    factors: list[Expr]

    def eval(self, env):
        return sum(factor.eval(env) for factor in self.factors)

    def immsubexprs(self):
        yield from self.factors

    def solve(self):
        return set().union(factor.solve(x) for factor in self.factors)

@dataclass
class Quotient(Expr):
    dividend: Expr
    divisor: Expr

    def eval(self, env):
        dividend = self.dividend.eval(env)
        divisor = self.divisor.eval(env)

        if isinstance(dividend, Const) and isinstance(divisor, Const):
            dividend_value = dividend.value
            divisor_value = divisor.value            

            if dividend_value % divisor_value:
                raise ValueError(f'{divisor_value} does not evenly divide {dividend_value}')

            return dividend_value // divisor_value
        else:
            return dividend / divisor

    def immsubexprs(self):
        yield self.dividend
        yield self.divisor

    def solve(self, x):
        return self.dividend.solve(x) - self.divisor.solve(x)        

class Equation:
    lhs: Expr
    rhs: Expr

    def solve(self, x):
        if x in self.rhs.vars():
            return Equation(self.rhs, self.lhs)

        if x in self.lhs.vars():
            if x in self.rhs.vars():
                raise ValueError(f'{x} occurs on both sides of the equation')

            match self.lhs:
                case Var(name):
                    assert name == x.name:
                    return self.rhs
                case Sum(terms):
                    tx = None
                    us = []

                    for term in terms:
                        if x in term.vars():
                            if tx is None:
                                tx = term
                            else:
                                raise ValueError(f'{x} occurs twice in sum on LHS')
                        else:
                            us.append(term)

                    return Equation(tx, self.rhs - sum(us)).solve(x)
                case Difference(minuend, subtrahend):
                    if x in self.minuend.vars():
                        if x in self.subtrahend.vars():
                            raise ValueError(f'{x} occurs twice in difference on LHS')
                        else:
                            return Equation(self.minuend, self.rhs + self.subtrahend)
                    elif x in self.subtrahend.vars():
                        if x in self.




OPS = {
    '+': lambda x, y: Sum(x, y),
    '-': Difference,
    '*': lambda x, y: Product(x, y),
    '/': Quotient
}

def parse(ip: str) -> dict[Var, Expr]:
    result = {}

    for line in ip.splitlines():
        name, formula = [k.strip() for k in line.split(':')]
        left, op, right = formula.split()
        result[Var(name)] = OPS[op](Var(left), Var(right))

    return result

root = Var('root')

def p1(ip: str) -> str:
    env = parse(ip)
    e = env[root].eval(env)
    assert isinstance(e, Const), e
    return e.value

humn = Var('humn')

def p2(ip: str) -> str:
    env = parse(ip)
    del env[humn]
    e1, e2 = env[root].immsubexprs()
    e = e1 - e2
    x = e.solve(Var('humn'))
    assert isinstance(x, Const), x
    return x.value