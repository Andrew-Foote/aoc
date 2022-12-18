import itertools as it
from typing import Iterator
import numpy as np

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

# if it was 2D
# then for each square, we could just check how many of the four adjacent squares are
#   non-squares
# alternatively, we could go through the corners and 

Cube = tuple[int, int, int]

def adj(cube: Cube) -> Iterator[Cube]:
    x, y, z = cube
    yield (x + 1, y, z)
    yield (x - 1, y, z)
    yield (x, y + 1, z)
    yield (x, y - 1, z)
    yield (x, y, z + 1)
    yield (x, y, z - 1)

def p1(ip: str) -> int:
    cubes = {tuple(map(int, line.split(','))) for line in ip.splitlines()}
    sa = 0

    for cube in cubes:
        sa += sum(1 for acube in adj(cube) if acube not in cubes)

    return sa

def inbounds(cube: Cube, mx: int, my: int, mz: int) -> bool:
    return 0 <= cube[0] < mx and 0 <= cube[1] < my and 0 <= cube[2] < mz

def adj_inbounds(cube: Cube) -> Iterator[Cube]:
    for acube in adj(cube):
        if inbounds(acube):
            yield acube

def is_reachable(cube: Cube, cubes: set[Cube], lbounds: Cube, ubounds: Cube) -> bool:
    # a non-cube point is non-reachable
    # if all adjacent points are either cubes or non-reachable
    # and it is reachable if it's outside of the bounds

    if cube in cubes:
        return False

    if (
        cube[0] < lbounds[0] or cube[1] < lbounds[1] or cube[2] < lbounds[3]
        or
        cube[0] >= ubounds[0] or cube[1] >= ubounds[1] or cube[1] >= ubounds[3]
    ):
        return True

    for acube in adj(cube):
        if is_reachable(acube):
            return True

    return False

def p2(ip: str) -> int:
    cubes = {tuple(map(int, line.split(','))) for line in ip.splitlines()}
    mx = max(x for x, y, z in cubes)
    my = max(y for x, y, z in cubes)
    mz = max(z for x, y, z in cubes)
    
    adj_not_cube = lambda p: filter(lambda q: q not in cubes, adj(p))
    acube_points = set(it.chain.from_iterable(adj_not_cube(cube) for cube in cubes))

    for apoint in acube_points:
        stack = [iter(adj_not_cube(apoint))]
        visited = {apoint}
        result = 'not_reachable'

        while stack:
            try:
                point = next(stack[-1])
            except StopIteration:
                del stack[-1]
            else:
                if not inbounds(point, mx, my, mz):
                    result = 'reachable'
                    break

                if point not in visited:
                    visited.add(point)
                    stack.append(iter(adj_not_cube(point)))

        if result == 'not_reachable':
            cubes.update(visited)

    sa = 0

    for cube in cubes:
        sa += sum(1 for acube in adj(cube) if acube not in cubes)

    return sa
