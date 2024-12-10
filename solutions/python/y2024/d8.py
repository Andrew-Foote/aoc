from collections import defaultdict
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid

test_inputs = [
    ('example', '''\
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
''', [
        ('antinodes_pic', '''\
......#....#
...#....0...
....#0....#.
..#....0....
....0....#..
.#....A.....
...#........
#......#....
........A...
.........A..
..........#.
..........#.'''),
        ('p1', 14),
        ('p2', 34)
    ]),
]

def parse(ip: str) -> Grid:
    return Grid(ip.splitlines())

# for any two cells with the same frequency, we draw a line
# between them, then extend by the same length each side
# and ptu the two antinodes at the ends... i think
# antinodes can be on the same place as a cell with a freq
# antinodes are marked with '#' in the answers

def map_freq_to_points(grid: Grid) -> dict[str, list[gint]]:
    result: defaultdict[str, list[gint]] = defaultdict(list)

    for point in grid.rect():
        freq = grid[point]

        if freq != '.':
            result[freq].append(point)

    return dict(result)

def p1(ip: str) -> int:
    grid = parse(ip)
    width = grid.width
    height = grid.height
    freq_to_points = map_freq_to_points(grid)
    antinode_locs: set[gint] = set()

    for _, points in freq_to_points.items():
        for i, p1 in enumerate(points):
            for p2 in points[i + 1:]:
                disp = p2 - p1
                an0 = p1 - disp
                an1 = p2 + disp

                if 0 <= an0.real < width and 0 <= an0.imag < height:
                    antinode_locs.add(an0)

                if 0 <= an1.real < width and 0 <= an1.imag < height:
                    antinode_locs.add(an1)

    return len(antinode_locs)

def p2(ip: str) -> int:
    grid = parse(ip)
    width = grid.width
    height = grid.height
    freq_to_points = map_freq_to_points(grid)
    antinode_locs: set[gint] = set()

    for _, points in freq_to_points.items():
        for i, p1 in enumerate(points):
            for p2 in points[i + 1:]:
                disp = p2 - p1
                an0 = p1
                an1 = p2

                while an0 in grid or an1 in grid:
                    if an0 in grid:
                        antinode_locs.add(an0)

                    if an1 in grid:
                        antinode_locs.add(an1)

                    an0 -= disp
                    an1 += disp

    return len(antinode_locs)
