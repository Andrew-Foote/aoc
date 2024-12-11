from collections import deque
from collections.abc import Generator
from enum import Enum
import functools as ft
from typing import assert_never, Self
import numpy as np
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW

test_inputs = [
    ('example', '''\
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........''', [
        ('reachable_count_6_steps', 16),
    ])
]

class Cell(Enum):
    PLOT = '.'
    ROCK = '#'

    @classmethod
    def parse(cls, char: str) -> Self:
        return cls(char)
    
    @property
    def char(self) -> str:
        return self.value
    
def parse(ip: str) -> tuple[Grid[Cell], gint]:
    start: gint

    def parse_cell(p: gint, char: str) -> Cell:
        nonlocal start

        match char:
            case 'S':
                start = p
                return Cell.PLOT
            case '.':
                return Cell.PLOT
            case '#':
                return Cell.ROCK
            case _:
                raise ValueError(f"unexpected character '{char}'")

    return Grid.parse(ip.splitlines(), parse_cell), start

# @ft.cache
# def reachable_cells(
#     grid: Grid[Cell], start: gint, steps: int
# ) -> Generator[Cell]:
    
#     #print(f'reachable_cells(grid, {start}, {steps})')
    
#     assert grid[start] == Cell.PLOT

#     if not steps:
#         yield start
#         return
    
#     for d in NESW:
#         neighbour = start + d

#         try:
#             neighbour_type = grid[neighbour]
#         except KeyError:
#             continue

#         match neighbour_type:
#             case Cell.PLOT:
#                 yield from reachable_cells(grid, neighbour, steps - 1)
#             case Cell.ROCK:
#                 continue
#             case _:
#                 assert_never(neighbour_type)

# def reachable_count(grid: Grid[Cell], start: gint, steps: int) -> int:
#     return len(set(reachable_cells(grid, start, steps)))

# {(0, 0)}
# {(0, -1), (0, 1), (1, 0), (-1, 0)}
# ({0, 0), })

# def reachable_count(grid: Grid[Cell], start: gint, steps: gint) -> int:
#     queue: deque[tuple[gint, int]] = deque([(start, steps)])
#     visited: dict[gint, int] = {}
#     count = 0

#     while queue:
#         print(queue)
#         cur, rem_steps = queue.pop()
#         print(f'can reach {cur} with {rem_steps} steps remaining')

#         # if we have already been able to get to this node with at least as
#         # many remaining steps, then we don't need to explore this node further
#         # however we do need to explore it if we have more remaining steps

#         if cur in visited and visited[cur] >= rem_steps:
#             # we have already counted all stopping points that involve passing
#             # through cur?
#             print(f'however we already counted all stopping points from {cur} with {visited[cur]} steps remaining')
#             continue
#         else:
#             visited[cur] = rem_steps

#         if rem_steps == 0:
#             print(f'found stopping point at {cur}')
#             count += 1
#             continue

#         for d in NESW:
#             neighbour = cur + d

#             try:
#                 neighbour_type = grid[neighbour]
#             except KeyError:
#                 continue

#             match neighbour_type:
#                 case Cell.PLOT:
#                     print(f'adding neighbour of {cur} in {d} dir: {neighbour}')
#                     queue.appendleft((neighbour, rem_steps - 1))
#                 case Cell.ROCK:
#                     continue
#                 case _:
#                     assert_never(neighbour_type)

#     return count




# for a given position, consider what numbers of steps it can be reached on
# the start position:
#  - it can be reached in precisely those numbers of steps that are EVEN
#    how to prove? well, it can definitely be reached in 0 steps
#    if any adjacent call can be reached on step n, it can be reached on
#    step n + 1
#     
# if a cell can be reached in n steps, it can't be reached in n + 1 steps
# why not? well consider the coords relative to the start
# in 0 steps, coords are (0, 0)
# a step must increase or decrease exactly one of these coordinates by 1
# so at n steps, each possible set of coordinates is of the form
# (a, b) where a is a sum of 1s and -1s, b is a sum of 1s and -1s, and
# the number of terms in a and b, in total, is n
# 
# let a1 be the number of 1s in a, let a2 be the number of -1s in a,
# let b1 be the number of 1s in b, let b2 be the number of -1s in b
# we have a = a1 - a2, b = b1 - b1, a1 + a2 + b1 + b2 = n
# 
# so cell (x, y) (where coords are relative to start) can be reached in n steps
# iff there exist non-negative integers a, b, c, d such that
# x = a - b, y = c - d, n = a + b + c + d
# a >= 0, b >= 0, c >= 0, d >= 0
# x + b >= 0, b >= 0, y + d >= 0, d >= 0
# b >= -x, b >= 0, d >= -y, d >= 0
# b >= max(-x, 0), d >= max(-y, 0)

# this is equivalent for asking to a solution to the following sys of linear equations:
#  a - b         = x
#          c - d = y
#  a + b + c + d = n
#
# which is equiv to
#
# a - b          = x
#          c - d = y
#     2b + c + d = n - x
#
# which is equiv to
#
# a - b          = x
#     2b + c + d = n - x
#          c - d = y
#
# (which is in row echelon form)
#
# a - b             = x
#     b + c/2 + d/2 = (n - x)/2
#         c   - d   = y
#
#
# a     + c/2 + d/2 = x + (n - x)/2
#     b + c/2 + d/2 = (n - x)/2
#         c   - d   = y
#
#
# a       + d = x + (n - x)/2 - y/2
#     b   + d = (n - x)/2 - y/2
#       c - d = y
#
#
# c = y + d
# b = (n - x - y)/2 - d
# a = x + (n - x - y)/2 - d
#   = (n + x - y)/2 - d
# 
# so if we choose d as a non-negative integer
# then c = y + d is also a non-negative integer as long as y + d >= 0, i.e. d >= -y
# and b = (n - x - y)/2 - d is also a non-negative integer as long as:
#   * n - x - y is even
#   * (n - x - y)/2 >= d
# and a = (n + x - y)/2 - d is also a non-negative integer as long as:
#   * n + x - y is even
#   * (n + x - y)/2 >= d
#
#  to summarize: the sys has a solution iff n - x - y and n + x - y are even,
#  and there exists an integer
#  d such that max(0, -y) <= d <= min[ (n - x - y)/2, (n + x - y)/2 ]
# i.e. max(0, -y) <= min [ (n - y)/2 - x/2, (n - y)/2 + x/2 ]
#  if y is non-negative this is equivalent to 0 <= min [ (n - x - y)/2, (n + x - y)/2 ]
#   which requires 0 <= (n - x - y)/2 and 0 <= (n + x - y)/2
#   i.e. 0 <= n - x - y and 0 <= n + x - y
#   i.e. y + x <= n and y - x <= n    max(x + y, y - x) <= n
#        x <= n - y and -x <= n - y    y - n <= x     y - n <= -x
#    y - n <= x <= n - y
#    abs(x) <= n - y

#  abs(u) <= v    <=>    -v <= u <= v


# (oh but they have to be integer solutions)
#
#
#  a 


#
#   a = x + b   a = n - b - c - d
#
#   c = y + d 
#
#   n = x + 2b + y + 2d

#   (note: this implies n - x - y = 2(b + d)
#                       n + x + y = 2(a + c)
#                       n + x - y = 2(a + d)
#                       n - x + y = 2(b + c)
#   so n - x - y, n - x + y, n + x - y, n - x + y must all be even and non-
#   negative
# conversely, if they're all even and non-negative we can take b and d to be
# any non-negative integers with b >= -x, d >= -y, and set a = x + b, c = y + d;
# 

# if we know b, then a must be x + d

# which means e.g. (1, 1) can't be reached in 1 step 

def reachable_count(grid: Grid[Cell], start: gint, steps: int) -> int:
    count = 0

    for p in grid.rect():
        x = p.real
        y = p.imag
        n = steps
        
        coeff = np.array([
            [1, -1, 0, 0],
            [0, 0, 1, -1],
            [1, 1, 1, 1]]
        )

        aug = np.hstack((coeff, np.array([[x, y, n]]).T))
        
        if np.linalg.matrix_rank(coeff) == np.linalg.matrix_rank(aug):
            count += 1

    return count

def reachable_count_6_steps(ip: str) -> int:
    grid, start = parse(ip)
    return reachable_count(grid, start, 6)

def p1(ip: str) -> int:
    grid, start = parse(ip)
    return reachable_count(grid, start, 64)