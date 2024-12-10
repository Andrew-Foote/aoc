from collections.abc import Generator
import functools as ft
from solutions.python.lib.gint import gint
from solutions.python.lib.graph import dfs
from solutions.python.lib.grid import Grid, NESW

test_inputs = [
    ('example', '''\
0123
1234
8765
9876''', [
        ('p1', 1),
    ]),
]

def parse(ip: str) -> Grid:
    return Grid(ip.splitlines()).map(int)

def children(grid: Grid, point: gint) -> Generator[gint]:
    height = grid[point]

    if height == 9:
        return

    for d in NESW:
        neighbour = point + d

        if neighbour in grid.rect() and grid[neighbour] == height + 1:
            yield neighbour

def p1(ip: str) -> int:
    grid = parse(ip)
    start_points = [p for p in grid.rect() if grid[p] == 0]

    scores = [len({
        p for p in dfs(start_point, ft.partial(children, grid))
        if grid[p] == 9
    }) for start_point in start_points]

    return sum(scores)
