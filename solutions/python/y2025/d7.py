import functools as ft
from solutions.python.lib.grid2 import Point, Rect
import solutions.python.lib.grid2 as g

test_inputs = [('example', '''\
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............''', [
    ('p1', 21),
    ('p2', 40),
])]

def parse(ip: str) -> tuple[Rect, Point, set[Point]]:
    start: Point | None = None
    splitters: set[Point] = set()

    lines = ip.splitlines()
    height = len(lines)
    assert lines
    width = len(lines[0])
    rect = Rect(Point(0, 0), Point(width, height))

    for y, line in enumerate(ip.splitlines()):
        for x, char in enumerate(line):
            p = Point(x, y)

            if char == 'S':
                start = p
            elif char == '^':
                splitters.add(p)
    
    assert start is not None 
    return rect, start, splitters

def p1(ip: str) -> int:
    rect, start, splitters = parse(ip)
    split_count: int = 0
    beams: set[Point] = {start}

    while beams:
        next_beams: set[Point] = set()

        for beam in beams:
            if beam in rect:
                next_beam = beam + g.SOUTH

                if next_beam in splitters:
                    split_count += 1
                    next_beams.update((next_beam + g.WEST, next_beam + g.EAST))
                else:
                    next_beams.add(next_beam)

        beams = next_beams       

    return split_count

def p2(ip: str) -> int:
    rect, start, splitters = parse(ip)

    @ft.cache
    def timeline_count(start: Point) -> int:
        if start not in rect:
            return 1
        
        nxt = start + g.SOUTH

        if nxt in splitters:
            return (
                timeline_count(nxt + g.WEST)
                + timeline_count(nxt + g.EAST)
            )
    
        return timeline_count(nxt)

    return timeline_count(start)