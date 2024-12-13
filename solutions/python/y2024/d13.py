from collections.abc import Generator
from dataclasses import dataclass
import math
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
    ('p2', 0),
])]

def cost(solution: tuple[int, int]) -> int:
    u, v = solution
    return u * 3 + v

@dataclass
class Machine:
    a_vec: gint
    b_vec: gint
    prize_pos: gint

    def _solutions(self, a: int, b: int, p: int) -> Generator[tuple[int, int]]:
        """Yields all 2-tuples (u, v) of integers such that a * u + b * v == p,
        0 <= u <= 100 and 0 <= v <= 100."""

        # To solve the equation a * u + b * v == p, we focus on solving for u
        # first, and rewrite it as a congruence: a * u == p (mod b). This can
        # be solved in a standard way, and it will either have no solutions, or
        # its solution set will be a residue class.

        sol = numth.solve_lincong(a, p, b)

        if sol is None:
            return

        # Once we have the solution set, we can use the inequalities to reduce
        # it to a finite set, and then just loop through each possible solution,
        # and calculate the unique value of v that will work with it for each
        # one.

        for u in sol.bound(0, 101):
            #     a * u + b * v == p 
            # <=> b * v == p - a * u
            # <=> v == (p - a * u) / b
            v = (p - a * u) // b
            yield (u, v)

    def x_solutions(self) -> Generator[tuple[int, int]]:
        """Yields all 2-tuples (u, v) of integers such that the machine will
        be at the prize's x-coordinate after u presses of button A and presses
        of button B."""

        yield from self._solutions(
            self.a_vec.real, self.b_vec.real, self.prize_pos.real
        )

    def y_solutions(self) -> Generator[tuple[int, int]]:
        """Yields all 2-tuples (u, v) of integers such that the machine will
        be at the prize's y-coordinate after u presses of button A and presses
        of button B."""

        yield from self._solutions(
            self.a_vec.imag, self.b_vec.imag, self.prize_pos.imag
        )

    def solutions(self) -> set[tuple[int, int]]:
        """Yields all 2-tuples (u, v) of integers such that the machine will
        be at the prize's coordinates after u presses of button A and presses
        of button B."""

        return set(self.x_solutions()) & set(self.y_solutions())

    def cheapest_solution(self) -> tuple[int, int] | None:
        sols = self.solutions()

        if not sols:
            return None
        
        return min(sols, key=cost)

def parse(ip: str) -> Generator[Machine]:
    paras = ip.split('\n\n')

    for para in paras:
        lines = para.splitlines()
        assert len(lines) == 3
        a_line, b_line, prize_line = lines

        m = re.match(r'Button A: X\+(\d+), Y\+(\d+)', a_line)
        assert m is not None, a_line
        a_x, a_y = map(int, m.groups())
        a_vec = gint(a_x, a_y)

        m = re.match(r'Button B: X\+(\d+), Y\+(\d+)', b_line)
        assert m is not None, b_line
        b_x, b_y = map(int, m.groups())
        b_vec = gint(b_x, b_y)

        m = re.match(r'Prize: X=(\d+), Y=(\d+)', prize_line)
        assert m is not None, prize_line
        p_x, p_y = map(int, m.groups())
        prize_pos = gint(p_x, p_y)

        yield Machine(a_vec, b_vec, prize_pos)

def p1(ip: str) -> int:
    result = 0

    for machine in parse(ip):
        sol = machine.cheapest_solution()

        if sol is not None:
            result += cost(sol)

    return result

def p2(ip: str) -> int:
    return 0