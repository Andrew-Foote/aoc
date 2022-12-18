import numpy as np

test_inputs = [('example1', '1,1,1\n2,1,1', [
    ('p1', 10),
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
    ('p1', 64)
])]

# if it was 2D
# then for each square, we could just check how many of the four adjacent squares are
#   non-squares
# alternatively, we could go through the corners and 

Cube = tuple[int, int, int]

def adj(cube: Cube) -> list[Cube]:
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
