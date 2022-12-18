import functools as ft
import itertools as it
from typing import Iterator
from solutions.python.lib import graph

test_inputs = [('example1', '1,1,1\n2,1,1', [
    ('p1', 10),
    ('p1', 10)
]), ('example2', '''\
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5\
''', [
    ('p1', 64),
    ('p2', 58)
])]

Cell = tuple[int, int, int]

def adj(cell: Cell) -> Iterator[Cell]:
    x, y, z = cell
    yield (x + 1, y, z)
    yield (x - 1, y, z)
    yield (x, y + 1, z)
    yield (x, y - 1, z)
    yield (x, y, z + 1)
    yield (x, y, z - 1)

def surface_area(cubes: set[Cell]) -> int:
    return sum(1 for cube in cubes for cell in adj(cube) if cell not in cubes)

def parse(ip: str) -> set[Cell]:
    cubes = set()

    for line in ip.splitlines():
        x, y, z = map(int, line.split(','))
        cubes.add((x, y, z))

    return cubes

def p1(ip: str) -> int:
    return surface_area(parse(ip))

def inbounds(cell: Cell, mx: int, my: int, mz: int) -> bool:
    return 0 <= cell[0] < mx and 0 <= cell[1] < my and 0 <= cell[2] < mz

def p2(ip: str) -> int:
    cubes = parse(ip)
    mx = max(x for x, y, z in cubes)
    my = max(y for x, y, z in cubes)
    mz = max(z for x, y, z in cubes)
    acells = {cell for cube in cubes for cell in adj(cube) if cell not in cubes}

    for cell in acells:
        search = graph.gdfs(cell, lambda cell: (acell for acell in adj(cell) if acell not in cubes))

        for point in search:
            if not inbounds(point, mx, my, mz):
                break
        else:
            cubes.update(search.visited)

    return surface_area(cubes)
