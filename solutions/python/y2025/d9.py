from collections.abc import Iterator
from solutions.python.lib.grid2 import Point

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
    ])
]

def parse(ip: str) -> Iterator[Point]:
    for line in ip.splitlines():
        x, y = line.split(',')
        yield Point(int(x), int(y))

def p1(ip: str) -> int:
    red_tiles = list(parse(ip))
    
    tile_combos = [
        (p1, p2) for i, p1 in enumerate(red_tiles) for p2 in red_tiles[i + 1:]
    ]

    areas: list[int] = []    

    for p1, p2 in tile_combos:
        width = abs(p1.x - p2.x) + 1
        height = abs(p1.y - p2.y) + 1
        area = width * height
        areas.append(area)

    return max(areas)