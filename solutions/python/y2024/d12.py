from collections import defaultdict
from collections.abc import Generator
from fractions import Fraction
import functools as ft
from pathlib import Path
import pickle
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW, NE_SE_SW_NW, Rect

test_inputs = [('example', '''\
AAAA
BBCD
BBCC
EEEC''', [
	('areas_csv', '4,4,4,1,3'),
    ('perimeters_csv', '10,8,10,4,8'),
    ('p1', 140),
    ('side_counts_csv', '4,4,8,4,4'),
    ('p2', 80),
]), ('example2', '''\
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO''', [
    ('areas_csv', '21,1,1,1,1'),
    ('perimeters_csv', '36,4,4,4,4'),
    ('p1', 772),
    ('p2', 436),
]), ('example3', '''\
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE''', [
    ('areas_csv', '12,4,14,14,1,10,13,11,13,5,3'),
    ('perimeters_csv', '18,8,22,28,4,18,20,20,18,12,8'),
    ('p1', 1930),
]), ('example4', '''\
III
IAI
IIA''', [
    ('areas_csv', '7,1,1'),
    ('perimeters_csv', '16,4,4'),
    ('p1', 120)
]), ('example5', '''\
AXAXAXA
AXAXAXA
AAAAAXA''', [
    ('areas_csv', '11,3,2,2,3'),
    ('perimeters_csv','24,8,6,6,8'),
    ('p1', 336),
]), ('example6', '''\
AAAAAAAA
AACBBDDA
AACBBAAA
ABBAAAAA
ABBADDDA
AAAADADA
AAAAAAAA''', [
    ('areas_csv', '39,2,4,4,2,5'),
    ('p2', 946)
])]

DEBUG = False

def regions(ip: str) -> Generator[tuple[str, set[gint]]]:
    grid = Grid(ip.strip().splitlines())
    if DEBUG: input()
    if DEBUG: print(grid.picture())
    regions: defaultdict[str, dict[gint, object]] = defaultdict(dict)
    group_parents: dict[object, object] = {}

    for p in grid.rect():
        plant_type = grid[p]
        if DEBUG: print(f'found point {p} of type {plant_type}')
        this_plant_regions = regions[plant_type]
        group: object = None

        for d in NESW:
            adj = p + d

            if adj in this_plant_regions:
                adj_group = this_plant_regions[adj]

                while adj_group in group_parents:
                    adj_group = group_parents[adj_group]

                if DEBUG: print(f'  adj point {adj} is in region {id(group)}')

                if group is not None and adj_group != group:
                    if DEBUG: print(f'  so linking groups {id(group)} and {id(adj_group)}')
                    group_parents[adj_group] = group
                else:
                    group = adj_group
                    this_plant_regions[p] = group
        
        if group is None:
            group = object()
            if DEBUG: print(f'  in a new region (ID {id(group)})')
            this_plant_regions[p] = group

    if DEBUG: print('  ---')

    for plant_type, this_plant_regions in regions.items():
        regions_as_sets: defaultdict[object, set[gint]] = defaultdict(set)

        for p, group in this_plant_regions.items():
            while group in group_parents:
                group = group_parents[group]

            regions_as_sets[group].add(p)

        for region in regions_as_sets.values():
            if DEBUG: print(f'  got {plant_type} region: {region}')
            yield plant_type, region

def reconstruct_ip(regions: Generator[tuple[str, set[gint]]]) -> str:
    grid_dict: dict[gint, str] = {}

    for plant_type, region in regions:
        for p in region:
            grid_dict[p] = plant_type

    w = max(p.real for p in grid_dict.keys()) + 1
    h = max(p.imag for p in grid_dict.keys()) + 1
    return Rect.from_tlwh(0, 0, w, h).picture(lambda p: grid_dict[p])

def area(region: set[gint]) -> int:
    return len(region)

def perimeter(region: set[gint]) -> int:
    return sum(4 - sum(1 for d in NESW if p + d in region) for p in region)

def side_count(region: set[gint]) -> int:
    # number of sides = number of corners (this is a hint i got from reddit)
    # so, each cell in the region has up to 4 corners
    # each corner corresponds to one of the diagonally adjacent cells
    # the corner is a real corner if the two cells between the diagonally
    # adjacent cell, and the original cell, are not in the region
    #    (regardless of whether the diag cell itself is in the region)
    # oh wait that's only valid for "outie" corners
    # we can also have corners where the diagonally adjacent cell is absent
    # but the others aren't
    #
    # Y/N = is there a corner in the middle of the 4 points
    # * = does this count as 2 corners?
    #
    #             *
    # Y  N  N  Y  Y  Y  Y  N
    # .. .. .# .# #. #. ## ##
    # .O #O .O #O .O #O .O ##
    #
    #   _      side count = 8
    #  |_|_    corner count = 8 only if we double-count the middle one
    #    |_|   so * should count as 2
    #
    # ah, but some of these corners will be counted multiple times, by different
    # cells
    # 
    # ..
    # .O will only be counted once by the O cell
    #
    # .#  from pov of tr # is .O = #. which counts as 1
    # #O                      ##   #O
    #
    #     from pov of bl # is .# = ## which counts as 1 
    #                         O#   .O
    #
    # #.  from pov of tl # is O. = .O = #. which also counts as 2
    # .O                      .#   #.   .O
    #

    n, e, s, w = NESW
    ne, se, sw, nw = NE_SE_SW_NW
    corners = [(ne, n, e), (se, s, e), (sw, s, w), (nw, n, w)]

    corner_map = {
        (False, False, False): Fraction(1),
        (False, False, True): Fraction(0),
        (False, True, False): Fraction(0),
        (False, True, True): Fraction(1, 3),
        (True, False, False): Fraction(1),
        (True, False, True): Fraction(1, 3),
        (True, True, False): Fraction(1, 3),
        (True, True, True): Fraction(0),
    }
 
    return sum(
        corner_map[p + c1 in region, p + c2 in region, p + c3 in region]
        for p in region for c1, c2, c3 in corners
    )

def areas_csv(ip: str) -> str:
    return ','.join(str(area(region)) for _, region in regions(ip))

def perimeters_csv(ip: str) -> str:
    return ','.join(str(perimeter(region)) for _, region in regions(ip))

def side_counts_csv(ip: str) -> str:
    return ','.join(str(side_count(region)) for _, region in regions(ip))

def p1(ip: str) -> int:
    # print('RECONSTRUCTION:')
    # print(reconstruct_ip(regions(ip)))
    # input()
    return sum(area(region) * perimeter(region) for _, region in regions(ip))

def p2(ip: str) -> int:
    return sum(area(region) * side_count(region) for _, region in regions(ip))
