from collections import defaultdict
from collections.abc import Generator
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW

test_inputs = [('example', '''\
AAAA
BBCD
BBCC
EEEC''', [
	('areas_csv', '4,4,4,1,3'),
    ('perimeters_csv', '10,8,10,4,8'),
    ('p1', 140)
]), ('example2', '''\
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO''', [
    ('areas_csv', '21,1,1,1,1'),
    ('perimeters_csv', '36,4,4,4,4'),
    ('p1', 772)
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
])]

def regions(ip: str) -> Generator[set[gint]]:
    grid = Grid(ip.splitlines())
    # input()
    # print(grid.picture())
    regions: dict[str, defaultdict[gint, object]] = defaultdict(dict)
    group_parents: dict[object, object] = {}

    for p in grid.rect():
        plant_type = grid[p]
        # print(f'found point {p} of type {plant_type}')
        this_plant_regions = regions[plant_type]
        group: object = None

        for d in NESW:
            adj = p + d

            if adj in this_plant_regions:
                adj_group = this_plant_regions[adj]
                # print(f'  adj point {adj} is in region {id(group)}')

                if group is not None and adj_group != group:
                    # print(f'  so linking groups {id(group)} and {id(adj_group)}')
                    group_parents[adj_group] = group
                else:
                    group = adj_group
                    this_plant_regions[p] = group
        
        if group is None:
            group = object()
            # print(f'  in a new region (ID {id(group)})')
            this_plant_regions[p] = group

    # print('  ---')

    for plant_type, this_plant_regions in regions.items():
        regions_as_sets: defaultdict[object, set[gint]] = defaultdict(set)

        for p, group in this_plant_regions.items():
            while group in group_parents:
                group = group_parents[group]

            regions_as_sets[group].add(p)

        for region in regions_as_sets.values():
            # print(f'  got {plant_type} region: {region}')
            yield region

def area(region: set[gint]) -> int:
    return len(region)

def perimeter(region: set[gint]) -> int:
    return sum(4 - sum(1 for d in NESW if p + d in region) for p in region)

def areas_csv(ip: str) -> str:
    return ','.join(str(area(region)) for region in regions(ip))

def perimeters_csv(ip: str) -> str:
    return ','.join(str(perimeter(region)) for region in regions(ip))

def p1(ip: str) -> int:
    return sum(area(region) * perimeter(region) for region in regions(ip))
    # ans 1513458 is too low