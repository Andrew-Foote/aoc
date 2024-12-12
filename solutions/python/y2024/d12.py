from collections import defaultdict
from collections.abc import Generator
import functools as ft
from pathlib import Path
import pickle
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import COMPASS, Grid, NESW, Rect

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
    ('areas_csv', '39,2,4,2,4,5'),
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

# (1, 2, 3, 4, 6, 7) corresponds to this picture:
#
# 1#2
# 3O4
# 567
#
# where O is a point, # is its north neighbour which definitely belongs to the
# region, and each numbered cell belongs to the region if the element with that
# region is True, and is outside the region if the element is False
Situation = tuple[bool, ...]

@ft.cache
def situations_path() -> Path:
    import os
    print(os.getcwd())
    input()
    return Path('solutions') / 'python' / 'y2024' / 'd12-situations.pkl'

@ft.cache
def situations_dict() -> dict[Situation, int]:
    sp = situations_path()

    if not sp.exists():
        return {}

    with sp.open('rb') as f:
        return pickle.load(f)

def save_situation_diff(situation: Situation, diff: int) -> None:
    sp = situations_path()
    sd = situations_dict()
    sd[situation] = diff

    with sp.open('wb') as f:
        pickle.dump(sd, f)

def side_count(region: set[gint], iter_region: set[gint] | None=None) -> int:
    if iter_region is None:
        iter_region = region.copy()

    print(f'side_count({region}, {iter_region})')

    if len(iter_region) == 1:
        print('len 1, so has 4 sides')
        return 4

    memb = iter_region.pop()
    
    rem_side_count = side_count(region, iter_region)

    # We know memb is within the region. Check whether each of the 8 surrounding
    # cells is also in the region.
    situation = tuple(memb + d in region for d in COMPASS)

    # In particular look at the 4 adjacent cells (the non-diagonal ones).
    # Since memb is within the region at least one of those 4 adjacent cells
    # must be within the region.
    for i in range(0, 8, 2):
        if situation[i]:
            break
    else:
        return rem_side_count + 4

    # Rotate the view so that the N cell is definitely within the region.
    # (The diff in number of sides is invariant under rotation. This just
    # reduces the number of situations I need to manually work out the diff
    # for.)
    situation = situation[i:] + situation[:i]
    sd = situations_dict()

    if situation in sd:
        return rem_side_count + sd[situation]

    def cellpic(cell: bool) -> str:
        return '🮋' if cell else ' '

    # just fill entries in manually
    print(situation)
    pic = '\n'.join([
        cellpic(situation[7]) + cellpic(situation[0]) + cellpic(situation[1]),
        cellpic(situation[2]) + '🮐' + cellpic(situation[3]),
        cellpic(situation[4]) + cellpic(situation[5]) + cellpic(situation[6]),
    ])

    print()
    print(pic)
    print()
    print(f'memb={memb}, rest={region}')
    side_diff = int(input('How many sides added? '))
    save_situation_diff(situation, side_diff)
    return rem_side_count + side_diff

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
