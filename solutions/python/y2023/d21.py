from collections.abc import Generator
from enum import Enum
from typing import assert_never, Self
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW
from solutions.python.lib.graph import gbfs

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

def reachable_count(grid: Grid, start: gint, steps: int) -> int:
    def children(node: tuple[gint, int]) -> Generator[tuple[gint, int]]:
        point, step_count = node

        if step_count >= steps:
            return

        for d in NESW:
            neighbour = point + d

            try:
                cell = grid[neighbour]
            except KeyError:
                continue

            match cell:
                case Cell.PLOT:
                    yield neighbour, step_count + 1
                case Cell.ROCK:
                    continue
                case _:
                    assert_never(cell)

    count = 0
    search = gbfs((start, 0), children)

    for point, step_count in search:
        if step_count >= steps:
            count += 1

    return count

def reachable_count_6_steps(ip: str) -> int:
    grid, start = parse(ip)
    return reachable_count(grid, start, 6)

def p1(ip: str) -> int:
    grid, start = parse(ip)
    return reachable_count(grid, start, 64)
