import functools as ft
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
    ('p2', '301')
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
    dicti = dict(parse(ip))

    def evalu(name, hmn):
        if name == 'humn':
            return hmn

        formula = dicti[name]

        try:
            v = int(formula)
        except ValueError:
            pass
        else:
            return v

        n1, op, n2 = formula.split(' ')

        if name == 'root':
            vv1 = evalu(n1, hmn)
            vv2 = evalu(n2, hmn)
            print(vv1, vv2)
            return vv1 == vv2

        return OPMAP[op](evalu(n1, hmn), evalu(n2, hmn))

    def formfor(name):
        if name == 'humn':
            return 'humn'

        formula = dicti[name]

        try:
            v = int(formula)
        except ValueError:
            pass
        else:
            return str(v)

        n1, op, n2 = formula.split(' ')
        if name == 'root': op = '-'
        subform1 = formfor(n1)
        subform2 = formfor(n2)
        bigform = f'({subform1}) {op} ({subform2})'
        
        if 'humn' not in bigform:
            return str(evalu(name, None))
        
        return bigform

    form = formfor('root')
    from sympy.parsing.sympy_parser import parse_expr
    parsed = parse_expr(form)
    print(parsed)

    # for i in range(1000_000, 1001_000):
    #     print(i, end = ' ; ')
    #     if evalu('root', i) == True:
    #         return i

    #         # right seems to fix at 22931068684876?


    #     # cpgd: humn - mplb
    #     # ppqt: vmnj * cpgd
    #     # znvn: gnvg + ppqt
    #     # wdgb: znvn / wwcj
    #     # tnnr: wdgb + ctnm
    #     # jspg: tnnr / zzhw
    #     # twrs: jspg - pbzb
    #     # hdgl: jbrv * twrs
    #     # qnhh: crqv + hdgl
    #     # vrjg: qnhh * ndpl