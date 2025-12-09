from collections.abc import Callable, Iterator
from dataclasses import dataclass
import functools as ft
import itertools as it
from typing import Self

@dataclass(frozen=True, slots=True)
class Vec:
    x: int
    y: int

    def rot_clockwise(self) -> 'Vec':
        return Vec(-self.y, self.x)

NORTH = Vec(0, -1)
EAST = Vec(1, 0)
SOUTH = Vec(0, 1)
WEST = Vec(-1, 0)
NESW = (NORTH, EAST, SOUTH, WEST)

@dataclass(frozen=True, slots=True, order=True)
class Point:
    x: int
    y: int

    def __iter__(self) -> Iterator[int]:
        yield self.x
        yield self.y

    def __add__(self, other: 'Vec') -> 'Point':
        return Point(self.x + other.x, self.y + other.y)
    
@dataclass(frozen=True, slots=True)
class Rect:
    # The coordinates of the cell INSIDE the rectangle and directly at its top
    # left.
    tl: Point

    # The coordinates of the cell OUTSIDE the rectangle and directly at its
    # bottom right.
    br: Point

    @property
    def left(self) -> int:
        return self.tl.x
     
    @property
    def right(self) -> int:
        return self.br.x
    
    @property
    def top(self) -> int:
        return self.tl.y
    
    @property
    def bottom(self) -> int:
        return self.br.y

    def __contains__(self, p: Point) -> bool:
        return self.left <= p.x < self.right and self.top <= p.y < self.bottom
    
    def __iter__(self) -> Iterator[Point]:
        for x in range(self.left, self.right):
            for y in range(self.top, self.bottom):
                yield Point(x, y)
    
    def picture(self, draw: Callable[[Point], str]) -> str:
        lines: list[str] = []

        for y in range(self.top, self.bottom):
            line_chars: list[str] = []
            
            for x in range(self.left, self.right):
                line_chars.append(draw(Point(x, y)))

            lines.append(''.join(line_chars))

        return '\n'.join(lines)

@dataclass(frozen=True, slots=True, eq=False)
class Grid:
    rows: list[list[str]]

    @property
    def width(self) -> int:
        return len(self.rows[0])

    @property
    def height(self) -> int:
        return len(self.rows)

    def __post_init__(self) -> None:
        if not self.rows:
            raise ValueError('empty grid')
        
        for row in self.rows[1:]:
            if len(row) != self.width:
                raise ValueError('grid rows differ in length')

    @ft.cache
    def rect(self) -> Rect:
        return Rect(Point(0, 0), Point(self.width, self.height))
    
    def __getitem__(self, p: Point) -> str:
        return self.rows[p.y][p.x]