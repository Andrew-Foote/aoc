from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from typing import Self
import re

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
    ('p2', 0)
])]

def parse(ip: str) -> Iterator[tuple[str, str]]:
    for line in ip.splitlines():
        name, formula = [k.strip() for k in line.split(':')]
        yield name, formula

OPMAP = {
    '+': lambda x, y: x + y,
    '*': lambda x, y: x * y,
    '-': lambda x, y: x - y,
    '/': lambda x, y: x // y
}

def p1(ip: str) -> str:
    dicti = dict(parse(ip))

    def evalu(name):
        formula = dicti[name]

        try:
            v = int(formula)
        except ValueError:
            pass
        else:
            return v

        n1, op, n2 = formula.split(' ')
        return OPMAP[op](evalu(n1), evalu(n2))

    return evalu('root')

def p2(ip: str) -> str:
    return 0
