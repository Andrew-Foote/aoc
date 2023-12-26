from collections import deque
import functools as ft
from typing import Iterator
import solutions.python.lib.grid as g
from solutions.python.lib.gint import gint
from solutions.python.lib.graph import gbfs

test_inputs = [
    ('example', '''\
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#''',
    [
        ('hike_lens_csv', '74,82,82,86,90,94'),
        ('p1', 94),
        ('p2', 154)
    ])
]

# path = .
# forest = #
# slopes = ^>v<

# there is exactly one path tile in the top row
# need to reach the unique path tile in the bottom row
# if you step on a slope, you go in the direction it's pointing
#   (this still counts as a step)
# you can't step on the same tile twice
# need to find the *longest* possible route

# so basically we have a network of nodes, corresponding
# to tiles, tiles generally connect to all non-forest adjacent
# tiles but if there's a slope they only connect to one
# (and the slope connections are directed)
# i guess you can't go up a slope either

# ok, so given a route, we can compute the possible routes after the next step

def parse_to_grid(ip: int) -> g.Grid:
    return g.Grid(ip.splitlines())

SLOPES = {
    '<': g.WEST,
    '>': g.EAST,
    '^': g.NORTH,
    'v': g.SOUTH
}

def parse_to_graph(ip: int, p2: bool=False) -> tuple[dict[gint, set[gint]], gint, gint]:
    grid = parse_to_grid(ip)
    graph = {}

    for point in grid.rect():
        v = grid[point]

        if v == '#':
            continue

        if v == '.':
            if point.imag == 0:
                start = point
            elif point.imag == grid.height - 1:
                end = point

        if v in SLOPES and not p2:
            neighbours = {point + SLOPES[v]}
        else:
            neighbours = {point + d for d in g.NESW}

        neighbours = {n for n in neighbours if n in grid.rect() and grid[n] != '#'}
        graph[point] = neighbours

    return graph, start, end

def parse_to_weighted_graph(ip: int, p2: bool=False) -> tuple[dict[gint, dict[gint, int]], gint, gint]:
    graph, start, end = parse_to_graph(ip, p2)
    junctions = set()

    for point, neighbours in graph.items():
        assert len(neighbours) > 0

        if len(neighbours) != 2:
            junctions.add(point)

    new_graph = {}

    for junction in junctions:
        new_graph[junction] = {}

        for neighbour in graph[junction]:
            point = junction
            n = neighbour
            d = 1

            while n not in junctions:
                nns = sorted(graph[n], key=lambda nn: nn == point)
                assert len(nns) == 2
                point = n
                n = nns[0]
                d += 1

            new_graph[junction][n] = d

    return new_graph, start, end

Path = tuple[frozenset[gint], gint, int]

def path_cons(path: Path, point: gint, weight: int) -> Path:
    return (path[0] | frozenset({path[1]}), point, weight)

def path_terminus(path: Path) -> gint:
    return path[1]

def path_length(path: Path) -> int:
    return path[2]

def path_contains(path: Path, point: gint) -> bool:
    return point in path[0] or point == path[1]

def hikes(ip: str, p2: bool=False) -> list[Path]:
    graph, start, end = parse_to_weighted_graph(ip, p2)

    def child_paths(path: Path) -> Iterator[Path]:
        t = path_terminus(path)
        l = path_length(path)

        for neighbour, weight in graph[t].items():
            if not path_contains(path, neighbour):
                yield path_cons(path, neighbour, l + weight)

    paths = [(frozenset(), start, 0)]
    successful_paths = []

    while paths:
        new_paths = []

        for path in paths:
            if path_terminus(path) == end:
                successful_paths.append(path)
            else:
                new_paths.extend(child_paths(path))

        print(len(new_paths), len(successful_paths))
        paths = new_paths

    return successful_paths

def hike_lens(ip: str, p2: bool=False) -> list[int]:
    return sorted(path_length(hike) for hike in hikes(ip, p2))

def hike_lens_csv(ip: str) -> str:
    return ','.join(map(str, hike_lens(ip)))

def p1(ip: str) -> int:
    return max(hike_lens(ip))

def p2(ip: str) -> int:
    return max(hike_lens(ip, True))
