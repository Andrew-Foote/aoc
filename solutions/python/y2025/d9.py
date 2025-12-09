from collections.abc import Iterable, Generator
from dataclasses import dataclass
from solutions.python.lib.graph import gbfs
from solutions.python.lib.grid2 import NESW, Point, Rect
from solutions.python.lib.utils import range_intersection

test_inputs = [
    ('example', '''\
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3''', [
        ('p1', 50),
        ('border_tiles_pic', '''\
..............
.......#XXX#..
.......X...X..
..#XXXX#...X..
..X........X..
..#XXXXXX#.X..
.........X.X..
.........#X#..
..............'''),
        ('green_tiles_pic', '''\
..............
.......#XXX#..
.......XXXXX..
..#XXXX#XXXX..
..XXXXXXXXXX..
..#XXXXXX#XX..
.........XXX..
.........#X#..
..............'''),
        ('p2', 24),
    ]),
    # https://old.reddit.com/r/adventofcode/comments/1pi36pq/2025_day_9_part_2_more_examples_to_soften_your/
    ('reddit1', '''\
4,2
13,2
13,4
8,4
8,6
11,6
11,10
4,10''', [
        ('p2', 40)
    ]),
    ('reddit2', '''\
3,2
13,2
13,4
8,4
8,6
11,6
11,11
7,11
7,8
5,8
5,10
3,10''', [
        ('p2', 35)
    ]),
    ('reddit3', '''\
3,2
17,2
17,13
13,13
13,11
15,11
15,8
11,8
11,15
18,15
18,17
4,17
4,12
6,12
6,5
3,5
''', [
        ('p2', 66)
])
]

def parse(ip: str) -> Generator[Point]:
    for line in ip.splitlines():
        x, y = line.split(',')
        yield Point(int(x), int(y))

def get_tile_combos(reds: list[Point]) -> Generator[tuple[Point, Point]]:
    for i, t1 in enumerate(reds):
        for t2 in reds[i + 1:]:
            yield t1, t2

def max_area(tile_combos: Iterable[tuple[Point, Point]]) -> int:
    areas: list[int] = []
    
    for t1, t2 in tile_combos:
        width = abs(t1.x - t2.x) + 1
        height = abs(t1.y - t2.y) + 1
        area = width * height
        areas.append(area)

    return max(areas)

def p1(ip: str) -> int:
    reds = list(parse(ip))
    tile_combos = get_tile_combos(reds)
    return max_area(tile_combos)

@dataclass
class Border:
    vlines: dict[int, tuple[int, int]]
    hlines: dict[int, tuple[int, int]]

    def __contains__(self, p: Point) -> bool:
        vlines = self.vlines
        hlines = self.hlines
        x, y = p

        if x in vlines:
            y0, y1 = vlines[x]

            if y0 <= y <= y1:
                return True
            
        if y in hlines:
            x0, x1 = hlines[y]

            if x0 <= x <= x1:
                return True
            
        return False

def get_border(reds: list[Point]) -> Border:
    vlines: dict[int, tuple[int, int]] = {}
    hlines: dict[int, tuple[int, int]] = {}

    for t1, t2 in zip(reds, [*reds[1:], reds[0]]):
        if t1.x == t2.x:
            y0, y1 = (t1.y, t2.y) if t1.y <= t2.y else (t2.y, t1.y)
            vlines[t1.x] = y0, y1
        elif t1.y == t2.y:
            x0, x1 = (t1.x, t2.x) if t1.x <= t2.x else (t2.x, t1.x)
            hlines[t1.y] = x0, x1

    return Border(vlines, hlines)

def border_tiles_pic(ip: str) -> str:
    reds = list(parse(ip))
    reds_set = set(reds)
    border = get_border(reds)
    r = Rect(Point(0, 0), Point(14, 9))

    def draw(p: Point) -> str:
        if p in reds_set:
            return '#'
        elif p in border:
            return 'X'
        else:
            return '.'

    return r.picture(draw)

# this turned out to be unnecessary for the solution
def get_greens(reds: list[Point]) -> set[Point]:
    border = get_border(reds)

    def neighbours(tile: Point) -> Generator[Point]:
        for d in NESW:
            nb = tile + d

            if nb not in border:
                yield nb

    starts: tuple[Point, Point] | None = None

    for x, (y0, y1) in border.vlines.items():
        if y0 + 1 < y1:
            poss_start1 = Point(x - 1, y0 + 1)
            poss_start2 = Point(x + 1, y0 + 1)

            if not (poss_start1 in border or poss_start2 in border):
                starts = (poss_start1, poss_start2)
                break

    assert starts is not None
    start1, start2 = starts
    search1 = gbfs(start1, neighbours)
    search2 = gbfs(start2, neighbours)

    while True:
        next1 = next(search1, None)
        next2 = next(search2, None)

        if next1 is None:
            return search1.visited
        elif next2 is None:
            return search2.visited

def green_tiles_pic(ip: str) -> str:
    reds = list(parse(ip))
    reds_set = set(reds)
    border = get_border(reds)
    greens = get_greens(reds)
    r = Rect(Point(0, 0), Point(14, 9))

    def draw(p: Point) -> str:
        if p in reds_set:
            return '#'
        elif p in border or p in greens:
            return 'X'
        else:
            return '.'

    return r.picture(draw)

def p2(ip: str) -> int:
    reds = list(parse(ip))
    border = get_border(reds)
    tile_combos = get_tile_combos(reds)
    good_tile_combos: list[tuple[Point, Point]] = []

    for t1, t2 in tile_combos:
        x0, x1 = (t1.x, t2.x) if t1.x <= t2.x else (t2.x, t1.x)
        y0, y1 = (t1.y, t2.y) if t1.y <= t2.y else (t2.y, t1.y)
        all_green = True

        # The idea is: if the rectangle formed by t1 and t2 has a border tile in
        # its interior it can't be all red and green tiles. (And versa versa,
        # if it has no border tiles in its interior it must be all red and green
        # tiles).
        # 
        # This is based on the assumption that the border doesn't "meet up with
        # itself", i.e. we can't have a situation like
        #
        # ..#----#..
        # ..|.##.|..
        # ..|.||.|..
        # ..#-##-#..
        # ..........
        #
        # The problem description doesn't really rule out this possibility, but
        # it works for my input.
        #
        # Semi-formal proof that the idea works: our assumption can be phrased
        # as: every border tile has at least one of its 8 neighbours neither red
        # nor green. So if a rectangle has a border tile in its interior then
        # that neighbour which is neither red nor green must be in the 
        # rectangle. Conversely, if there aren't any border tiles in the
        # rectangle's interior then there are certainly some on the boundary
        # (since the rectangle has two red tiles as corners, and those are on
        # the border) so the border must lie entirely either outside or on the
        # boundary of the rectangle which means the rectangle is within the
        # border and so is all red/green.
        #
        # A vertical line segment with x-coordinate X and y-coordinates from Y0
        # to Y1 (inclusive) will intersect the rect (x0, x1) x (y0, y1) iff
        # x0 < X < x1 and the intervals (y0, y1) and [Y0, Y1] intersect.
        #
        # A horizontal line segment with y-coordinate Y and x-coordinates from
        # X0 to X1 (inclusive) will intersect the rect (x0, x1) x (y0, y1) iff
        # y0 < Y < y1 and the intervals (x0, x1) and [X0, X1] intersect.

        for X, (Y0, Y1) in border.vlines.items():
            if X in range(x0 + 1, x1) and range_intersection(
                range(Y0, Y1 + 1), range(y0 + 1, y1)
            ):
                all_green = False
                break

        for Y, (X0, X1) in border.hlines.items():
            if Y in range(y0 + 1, y1) and range_intersection(
                range(X0, X1 + 1), range(x0 + 1, x1)
            ):
                all_green = False
                break 

        if all_green:
            good_tile_combos.append((t1, t2))

    return max_area(good_tile_combos)
