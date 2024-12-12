from collections import deque
from collections.abc import Generator
from enum import Enum
import functools as ft
from typing import assert_never, Self
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW
from solutions.python.lib.graph import dijkstra

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
		('reachable_count_10_steps', 50),
		('reachable_count_50_steps', 1594),
		('reachable_count_100_steps', 6536),
		('reachable_count_500_steps', 167004),
		('reachable_count_1000_steps', 668697),
		('reachable_count_5000_steps', 16733044)
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

# step count, then point
# step count comes first for dijkstra ordering
Node = tuple[int, gint]

def get_children(
    grid: Grid, avail_steps: int | None, node: Node, p2: bool
) -> Generator[Node]:

    step_count, point = node

    if avail_steps is not None and step_count >= avail_steps:
        return
    
    for d in NESW:
        neighbour = point + d

        if p2:
            nx = neighbour.real % grid.width
            ny = neighbour.imag % grid.height
            cell = grid[gint(nx, ny)]
        else:
            try:
                cell = grid[neighbour]
            except KeyError:
                continue

        match cell:
            case Cell.PLOT:
                yield step_count + 1, neighbour
            case Cell.ROCK:
                continue
            case _:
                assert_never(cell)

def reachable_count(grid: Grid, start: gint, steps: int, *, p2: bool) -> int:
    # first, we build up the `visted` set which is the set of all points on the
    # graph that are reachable with n steps *or less*
    queue = deque([(0, start)])
    visited: set[gint] = {start}

    while queue:
        step_count, point = queue.pop()
        children = get_children(grid, steps, (step_count, point), p2)

        for neighbour_step_count, neighbour in children:
            if neighbour not in visited:
                visited.add(neighbour)
                queue.appendleft((neighbour_step_count, neighbour))

    # then to get the count of those which are reachable with *exactly* n steps,
    # we make use of the fact that these form a "chess board" pattern within the
    # visited set, whose colours swap around with each step
    # (i haven't formally proved this, but it seems true empirically)
    # when the number of steps is 0, the only reachable point is the start one
    # so:
    # - if the number of steps is even, then the reachable points within the
    #   visited set, are all those such that the sum of their coordinates
    #   (relative to the start point) is even
    # - if the number of steps id odd, the reachable points are those where the
    #   coordinate sum is odd

    return sum(1 for p in visited if sum((p - start).rect()) % 2 == steps % 2)

def reachable_count_n_steps(ip: str, n: int) -> int:
    grid, start = parse(ip)
    return reachable_count(grid, start, n, p2=False)

def p2_reachable_count_n_steps(ip: str, n: int) -> int:
    # ok so for p2, n is so huge that above approach still won't work
    # however, if a grid is "fully contained" within the visited set then
    # its number of reachable points is going to be just the above some
    # for each non-obstalce point in the grid
    # so we could multipyl

    grid, start = parse(ip)
    # return reachable_count(grid, start, n, p2=True)

    # consider each border point on the grid which is not an obstacle
    # we can use the formula above to work out whether this point is reachable
    # for a given number of steps
    # well, i guess we also need to do a serch to find the ssmallest number
    # of steps it'll be reachable at

    # then we can buidl like a graph of grid-to-grid connections
    # (where a grid is really a start point, since it's the same grid each time)

    border_points = [
        p for p in grid.rect() if (
            p.real == 0 or p.imag == 0
            or p.real == grid.width - 1 or p.imag == grid.height - 1
        )
    ]

    border_points_set = set(border_points)

    poss_starts = [start] + border_points
    # maps each poss srtart to a dict mapping each point in the grid to the min
    # steps it takes to get from the poss start to that point
    dist_map: dict[gint, dict[gint, int]] = {}

    for poss_start in poss_starts:
        this_dist_map = {}
        dist_map[poss_start] = this_dist_map
        queue = deque([poss_start])
        children_map = ft.partial(get_children, grid, None, p2=False)

        for step_count, point in dijkstra((0, poss_start), children_map):
            if point not in dist_map:
                this_dist_map[point] = step_count



reachable_count_6_steps = ft.partial(reachable_count_n_steps, n=6)
p1 = ft.partial(reachable_count_n_steps, n=64)

reachable_count_10_steps = ft.partial(p2_reachable_count_n_steps, n=10)
reachable_count_50_steps = ft.partial(p2_reachable_count_n_steps, n=50)
reachable_count_100_steps = ft.partial(p2_reachable_count_n_steps, n=100)
reachable_count_500_steps = ft.partial(p2_reachable_count_n_steps, n=500)
reachable_count_1000_steps = ft.partial(p2_reachable_count_n_steps, n=1000)
reachable_count_5000_steps = ft.partial(p2_reachable_count_n_steps, n=5000)
p2 = ft.partial(reachable_count_n_steps, n=26501365)