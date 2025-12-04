from collections.abc import Callable, Iterator
from dataclasses import dataclass

test_inputs = [('example', '''\
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.''', [
    ('accessible_rolls_pic', '''\
..xx.xx@x.
x@@.@.@.@@
@@@@@.x.@@
@.@@@@..@.
x@.@@@@.@x
.@@@@@@@.@
.@.@.@.@@@
x.@@@.@@@@
.@@@@@@@@.
x.x.@@@.x.'''),
    ('p1', 13),
    ('accessible_roll_counts_csv', '13,12,7,5,2,1,1,1,1'),
    ('p2', 43)
])]

@dataclass(slots=True, frozen=True)
class Point:
    x: int
    y: int

    def __iter__(self) -> Iterator[int]:
        yield from (self.x, self.y)

@dataclass(slots=True, frozen=True)
class Grid:
    width: int
    height: int
    points: set[Point]

    def picture(self, f: Callable[[Point], str]) -> str:
        lines: list[str] = []

        for y in range(self.height):
            line: list[str] = []

            for x in range(self.width):
                line.append(f(Point(x, y)))
        
            lines.append(''.join(line))
    
        return '\n'.join(lines)

def adjacent_points(p: Point) -> list[Point]:
    x, y = p

    return [Point(x1, y1) for x1, y1 in (
        (x - 1, y - 1),
        (x - 1, y),
        (x - 1, y + 1),
        (x    , y - 1),
        (x    , y + 1),
        (x + 1, y - 1),
        (x + 1, y),
        (x + 1, y + 1)
    )]

def parse(ip: str) -> Grid:
    lines = ip.splitlines()
    width = len(lines[0])
    height = len(lines)
    points: set[Point] = set()

    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            if c == '@':
                points.add(Point(x, y))

    return Grid(width, height, points)

def accessible_rolls(grid: Grid) -> Iterator[Point]:
    for p in grid.points:
        adjacent_roll_count = sum(
            1 for q in adjacent_points(p) if q in grid.points
        )

        accessible = adjacent_roll_count < 4
        
        if accessible:
            yield p

def accessible_rolls_pic(ip: str) -> str:
    grid = parse(ip) 
    accessible = set(accessible_rolls(grid))
    
    def icon(p: Point) -> str:
        if p in accessible:
            return 'x'
        elif p in grid.points:
            return '@'
        else:
            return '.'
        
    return grid.picture(icon)

def p1(ip: str) -> int:
    return sum(1 for _ in accessible_rolls(parse(ip)))

def accessible_roll_counts(grid: Grid) -> Iterator[int]:
    accessible: set[Point]

    while True:
        accessible = set(accessible_rolls(grid))

        if accessible:
            yield len(accessible)

            for p in accessible:
                grid.points.remove(p)
        else:
            break

def accessible_roll_counts_csv(ip: str) -> str:
    grid = parse(ip)
    return ','.join(map(str, accessible_roll_counts(grid)))

def p2(ip: str) -> int:
    grid = parse(ip)
    return sum(accessible_roll_counts(grid))