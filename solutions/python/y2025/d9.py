from collections.abc import Iterable, Generator
from dataclasses import dataclass
import itertools as it
from solutions.python.lib.graph import gbfs
from solutions.python.lib.grid2 import NESW, Point, Rect

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

        # print('SEARCH1')
        # print(Rect(Point(0, 0), Point(14, 9)).picture(lambda p: '#' if p in border else 'X' if p in search1.visited else '.'))
        # print()
        # print('SEARCH2')
        # print(Rect(Point(0, 0), Point(14, 9)).picture(lambda p: '#' if p in border else 'X' if p in search2.visited else '.'))

        # input()
        if len(search1.visited) % 1000 == 0:
            print(len(search1.visited))

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

from solutions.python.lib.utils import range_intersection

def p2(ip: str) -> int:
    reds = list(parse(ip))
    border = get_border(reds)
    # print('getting greens...')
    # greens = get_greens(reds)

    # any way to avoid looping through all of these?
    tile_combos = get_tile_combos(reds)
    good_tile_combos: list[tuple[Point, Point]] = []

    for t1, t2 in tile_combos:
        print(f'checking {t1}, {t2}')
        x0, x1 = (t1.x, t2.x) if t1.x <= t2.x else (t2.x, t1.x)
        y0, y1 = (t1.y, t2.y) if t1.y <= t2.y else (t2.y, t1.y)
        all_green = True

        # if it has a border tile in its interior, it's not good
        for X, (Y0, Y1) in border.vlines.items():
            # we want to check if there's a point (x, y) in the rect interior
            # with x = X and Y0 <= y <= Y1
            # being in the rect interior means x0 < x < x1 and y0 < y < y1
            # so x = X will be equivalent to x0 < X < x1
            # and y0 < y < y1 will be equivalent to y in (y0, y1) nn [Y0, Y1]
            # so we want to check if (x0, x1) contains X, and [Y0, Y1] intersects (y0, y1)
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



        # for x, y in it.product(range(x0 + 1, x1), range(y0 + 1, y1)):
        #     t = Point(x, y)

        #     if t in border:
        #         all_green = False
        #         break

        # for x, y in it.product(range(x0, x1 + 1), range(y0, y1 + 1)):
        #     t = Point(x, y)
            
        #     if t not in border and t not in greens:
        #         all_green = False
        #         break

        if all_green:
            good_tile_combos.append((t1, t2))

    return max_area(good_tile_combos)
