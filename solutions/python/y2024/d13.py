from collections.abc import Generator
from dataclasses import dataclass
import math
import numpy as np
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

        ax, ay = self.a_vec.rect()
        bx, by = self.b_vec.rect()
        px, py = self.prize_pos.rect()

        # We want to find non-negative integers (u, v) such that
        # 
        #   ax * u + bx * v == px,  (1)
        #   ay * u + by * v == py.  (2)
        #
        # This is just a system of two linear equations. However we need the
        # integer solutions only, and using Numpy doesn't work for part 2 due to
        # precision issues, so we'll solve it manually.

        u, v = numth.line_intersection((ax, bx, px), (ay, by, py))
        print(u, v)

        if u.denominator == 1 and v.denominator == 1:
            return {(u.numerator, v.numerator)}

    def cheapest_solution(self) -> tuple[int, int] | None:
        sols = self.solutions()

        if not sols:
            return None
        
        return min(sols, key=cost)
    
    def p2_cheapest_solution(self) -> tuple[int, int] | None:
        coeff_matrix = np.array([
            [self.a_vec.real, self.b_vec.real],
            [self.a_vec.imag, self.b_vec.imag]
        ])

        rhs_vector = np.array([self.prize_pos.real, self.prize_pos.imag])

        print('coeffs', coeff_matrix)
        print('rhs', rhs_vector)

        sol = np.linalg.solve(coeff_matrix, rhs_vector)

        print(sol)

        u, v = sol
        ui = int(u)
        vi = int(v)

        if ui == u and vi == v:
            return {(ui, vi)}
        else:
            return None

        # set of all integers u such that for some integer v, the machine will
        # be at the prize's x-coordinate after u presses of button A and v
        # presses of button B
        xu_sol = numth.solve_lincong(
            self.a_vec.real, self.b_vec.real, self.prize_pos.real
        )

        # set of all integers u such that for some integer v, the machine will
        # be at the prize's y-coordinate after u presses of button A and v
        # presses of button B
        yu_sol = numth.solve_lincong(
            self.a_vec.imag, self.b_vec.imag, self.prize_pos.imag 
        )

        # set of all integers u such that for some integers v1 and v2, the
        # machine will be at the prize's x-coordinate after u presses of button
        # A and v1 presses of button B, and at the prize's y-coordinate after
        # u presses of button A and v2 presses of button B
        sol = xu_sol & yu_sol
        minsol = sol.min_given_lb(0)

        # we know that the u-solutions form an arithmetic progression
        # so there is a consistent gap between solutions
        # (u, v0) vs (u + m, v1)
        # 3 * u + v0 >= 3 * (u + m) + v1
        # is equiv to v1 - v0 <= -3 * m
        #
        # now, for each u-solution, the corresponding v-solutions will be
        # (p - a * u) // b   [over both x and y]
        #
        # we need the vs to be the same --- so
        #  (px - ax * u) // bx == (py - ay * u) // by
        # px // bx - ax * u // bx == py // by - ay * u // by
        # (ay // by - ax // bx) * u == py // by - px // bx
        # u == (py // by - px // bx) / (ay // by - ax // bx)?

        

        return (xu_sol & yu_sol).min_given_lb(0)

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

def p2_parse(ip: str) -> Generator[Machine]:
    inc = gint(10000000000000, 10000000000000)

    for machine in parse(ip):
        yield Machine(machine.a_vec, machine.b_vec, machine.prize_pos + inc)

def winnabilities_csv(ip: str) -> str:
    return ','.join(
        str(int(machine.cheapest_solution() is not None))
        for machine in parse(ip)
    )

def p1(ip: str) -> int:
    result = 0

    for machine in parse(ip):
        sol = machine.cheapest_solution()

        if sol is not None:
            result += cost(sol)

    return result

def p2_winnabilities_csv(ip: str) -> str:
    return ','.join(
        str(int(machine.cheapest_solution() is not None))
        for machine in p2_parse(ip)
    )

def p2(ip: str) -> int:
    result = 0

    for machine in p2_parse(ip):
        sol = machine.cheapest_solution()

        if sol is not None:
            result += cost(sol)

    return result
