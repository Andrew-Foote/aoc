from collections.abc import Generator
from collections import defaultdict
from dataclasses import dataclass
from enum import Enum
import functools as ft
import itertools as it
import math
from typing import assert_never, Self
from solutions.python.lib.gint import gint

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

def parse(ip: str) -> tuple[int, int, dict[gint, str]]:
    result = {}
    lines = ip.splitlines()

    for i, line in enumerate(lines):
        for j, col in enumerate(line):
            # a lowercase letter, uppercase letter or digit
            # this is the frequency
            result[gint(j, i)] = col

    h = len(lines)
    w = len(lines[0])
    return w, h, result

# for any two cells with the same frequency, we draw a line
# between them, then extend by the same length each side
# and ptu the two antinodes at the ends... i think
# antinodes can be on the same place as a cell with a freq
# antinodes are marked with '#' in the answers

def p1(ip: str) -> int:
    # we need to count number of locations that contain
    # an antinode
    width, height, grid = parse(ip)
    freq_to_points: defaultdict[str, set[gint]] = defaultdict(set)

    for point, freq in grid.items():
        if freq != '.':
            freq_to_points[freq].add(point)

    antinode_locs: set[gint] = set()

    for freq, points in freq_to_points.items():
        points_l = list(points)

        for i, p1 in enumerate(points_l):
            for p2 in points_l[i + 1:]:
                disp = p2 - p1
                an0 = p1 - disp
                an1 = p2 + disp

                if 0 <= an0.real < width and 0 <= an0.imag < height:
                    antinode_locs.add(an0)

                if 0 <= an1.real < width and 0 <= an1.imag < height:
                    antinode_locs.add(an1)

    return len(antinode_locs)

def p2(ip: str) -> int:
    width, height, grid = parse(ip)
    freq_to_points: defaultdict[str, set[gint]] = defaultdict(set)

    for point, freq in grid.items():
        if freq != '.':
            freq_to_points[freq].add(point)

    antinode_locs: set[gint] = set()

    for freq, points in freq_to_points.items():
        points_l = list(points)

        def inbounds(p: gint) -> bool:
            return 0 <= p.real < width and 0 <= p.imag < height

        for i, p1 in enumerate(points_l):
            for p2 in points_l[i + 1:]:
                disp = p2 - p1
                an0 = p1
                an1 = p2

                # if an0 == an1:
                #     antinode_locs.append(p1)
                #     continue

                while inbounds(an0) or inbounds(an1):
                    if inbounds(an0):
                        antinode_locs.add(an0)

                    if inbounds(an1):
                        antinode_locs.add(an1)

                    an0 -= disp
                    an1 += disp

    return len(antinode_locs)
