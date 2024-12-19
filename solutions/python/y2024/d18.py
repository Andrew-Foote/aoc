from collections import deque
from collections.abc import Generator
import itertools as it
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import NESW, Rect

test_inputs = [('example', '''\
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0

6''', [
    ('pic_12_falls', '''\
...#...
..#..#.
....#..
...#..#
..#..#.
.#..#..
#.#....'''),
    ('shortest_path_len_12_falls', '22'),
    ('p2', '6,1'),
])]

def parse(ip: str) -> tuple[list[gint], int]:
    parts = ip.split('\n\n')

    match parts:
        case [lines]:
            size = 70
        case [lines, size_s]:
            size = int(size_s)
        case _:
            assert False

    points: list[gint] = []

    for line in lines.splitlines():
        x, y = line.split(',')
        points.append(gint(int(x), int(y)))

    return points, size

def pic_12_falls(ip: str) -> int:
    points, size = parse(ip)
    fallen = set(points[:12])

    def draw_cell(pos: gint) -> str:
        return '#' if pos in fallen else '.'

    return Rect.from_tlwh(0, 0, size + 1, size + 1).picture(draw_cell)

def shortest_path_len_n_falls(
    points: list[int], size: int, fall_count: int
) -> int | None:

    fallen = set(points[:fall_count])
    start = gint(0, 0)
    end = gint(size, size)
    rect = Rect.from_tlwh(0, 0, size + 1, size + 1)
    queue: deque[tuple[int, gint]] = deque([(0, start)])
    visited: set[gint] = {start}

    while queue:
        steps, pos = queue.pop()

        if pos == end:
            return steps

        for d in NESW:
            adj = pos + d

            if adj not in rect or adj in fallen:
                continue

            if adj not in visited:
                queue.appendleft((steps + 1, adj))
                visited.add(adj)

    return None
        
def shortest_path_len_12_falls(ip: str) -> int | None:
    points, size = parse(ip)
    return shortest_path_len_n_falls(points, size, 12)

def p1(ip: str) -> int | None:
    points, size = parse(ip)
    return shortest_path_len_n_falls(points, size, 1024)

# bruteforce solution but it works ok
def p2(ip: str) -> int:
    points, size = parse(ip)

    for i, p in enumerate(points):
        if shortest_path_len_n_falls(ip, i + 1) is None:
            return f'{p.real},{p.imag}'
