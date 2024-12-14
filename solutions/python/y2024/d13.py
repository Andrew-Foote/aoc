from collections.abc import Generator
from dataclasses import dataclass
import functools as ft
import re
from solutions.python.lib.gint import gint
from solutions.python.lib import numth 

test_inputs = [('example', '''\
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279''', [
    ('p1', 480),
    ('winnabilities_csv', '1,0,1,0'),
    ('p2_winnabilities_csv', '0,1,0,1'),
]), ('example2', '''\
Button A: X+1, Y+1
Button B: X+1, Y+1
Prize: X=1, Y=1''', [
    ('p1', 1),
    ('winnabilities_csv', '1'),
])]

def cost(solution: tuple[int, int]) -> int:
    u, v = solution
    return u * 3 + v

@dataclass(frozen=True)
class Machine:
    a_inc: gint
    b_inc: gint
    prize_pos: gint
    
    @ft.cache
    def cheapest_solution(self) -> tuple[int, int] | None:
        ax, ay = self.a_inc.rect()
        bx, by = self.b_inc.rect()
        px, py = self.prize_pos.rect()

        # Let u be the number of times the A button is pressed, and let v be the
        # number of times the B button is pressed. The position of the claw will
        # then be (ax * u + bx * v, ay * u + by * v). So the prize is won iff
        # (u, v) is a solution to the system of 2 simultaneous equations
        #
        #   ax * u + bx * v == px,  (1)
        #   ay * u + by * v == py.  (2)
        #
        # Note that u and v also have to be non-negative integers.

        sol = numth.line_intersection((ax, bx, px), (ay, by, py))

        # We assume the system has exactly one solution. Although the problem
        # talks about the "cheapest" solution, it turns out that all the systems
        # we have to solve do have exactly one solution, so we don't actually
        # need to do any working out of which one is cheapest...

        assert sol is not None

        u, v = sol

        if (
            u.denominator == 1 and v.denominator == 1
            and u.numerator >= 0 and v.numerator >= 0
        ):
            return (u.numerator, v.numerator)
        
        return None

def parse(ip: str) -> Generator[Machine]:
    paras = ip.split('\n\n')

    for para in paras:
        lines = para.splitlines()
        assert len(lines) == 3
        a_line, b_line, prize_line = lines

        m = re.match(r'Button A: X\+(\d+), Y\+(\d+)', a_line)
        assert m is not None, a_line
        a_x, a_y = map(int, m.groups())
        a_inc = gint(a_x, a_y)

        m = re.match(r'Button B: X\+(\d+), Y\+(\d+)', b_line)
        assert m is not None, b_line
        b_x, b_y = map(int, m.groups())
        b_inc = gint(b_x, b_y)

        m = re.match(r'Prize: X=(\d+), Y=(\d+)', prize_line)
        assert m is not None, prize_line
        p_x, p_y = map(int, m.groups())
        prize_pos = gint(p_x, p_y)

        yield Machine(a_inc, b_inc, prize_pos)

def p2_parse(ip: str) -> Generator[Machine]:
    inc = gint(10000000000000, 10000000000000)

    for machine in parse(ip):
        yield Machine(machine.a_inc, machine.b_inc, machine.prize_pos + inc)

def winnabilities_csv(ip: str) -> str:
    return ','.join(
        str(int(machine.cheapest_solution() is not None))
        for machine in parse(ip)
    )

def optimal_cost(machines: Generator[Machine]) -> int:
    result = 0

    for machine in machines:
        sol = machine.cheapest_solution()

        if sol is not None:
            result += cost(sol)

    return result

def p1(ip: str) -> int:
    return optimal_cost(parse(ip))

def p2_winnabilities_csv(ip: str) -> str:
    return ','.join(
        str(int(machine.cheapest_solution() is not None))
        for machine in p2_parse(ip)
    )

def p2(ip: str) -> int:
    return optimal_cost(p2_parse(ip))