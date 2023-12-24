import functools as ft
from typing import Iterator
import solutions.python.lib.grid as g
from solutions.python.lib.gint import gint

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

def parse(ip: int) -> g.Grid:
	return g.Grid(ip.splitlines())

SLOPES = {
	'<': g.WEST,
	'>': g.EAST,
	'^': g.NORTH,
	'v': g.SOUTH
}

def accessible_neighbours(grid: g.Grid, point: gint, p2: bool=False) -> Iterator[gint]:
	v = grid[point]

	if v in SLOPES and not p2:
		neighbours = [point + SLOPES[v]]
	else:
		neighbours = [point + d for d in g.NESW]

	for n in neighbours:
		if n in grid.rect() and grid[n] != '#':
			yield n

Path = tuple[frozenset[gint], gint]

@ft.cache
def possible_path_extensions(grid: g.Grid, path: Path, p2: bool=False) -> list[Path]:
	result = []

	for neighbour in accessible_neighbours(grid, path[1], p2):
		if neighbour not in path[0] and neighbour != path[1]:
			result.append((path[0] | frozenset({path[1]}), neighbour))

	while len(result) == 1 and result[0][1].imag != grid.height - 1:
		path = (path[0] | frozenset({path[1]}), result[0][1])
		result = []

		for neighbour in accessible_neighbours(grid, path[1], p2):
			if neighbour not in path[0] and neighbour != path[1]:
				result.append((path[0] | frozenset({path[1]}), neighbour))

	return result

def find_start(grid: g.Grid):
	for i in range(grid.width):
		if grid[gint(i, 0)] == '.':
			return gint(i, 0)

def find_end(grid: g.Grid):
	for i in range(grid.width):
		if grid[gint(i, grid.height - 1)] == '.':
			return gint(i, grid.height - 1)

def hikes(grid: g.Grid, p2: bool=False) -> list[list[gint]]:
	start = find_start(grid)
	end = find_end(grid)

	paths = [ (frozenset(), start) ]
	successful_paths = []

	while paths:
		new_paths = []

		for path in paths:
			if path[1] == end:
				successful_paths.append(path)
			else:
				new_paths.extend(possible_path_extensions(grid, path, p2))

		print(len(new_paths), len(successful_paths))
		paths = new_paths

	return successful_paths

def hike_lens(ip: str, p2: bool=False) -> list[int]:
	return sorted(len(hike[0]) for hike in hikes(parse(ip), p2))

def hike_lens_csv(ip: str) -> str:
	return ','.join(map(str, hike_lens(ip)))

def p1(ip: str) -> int:
	return max(hike_lens(ip))

def p2(ip: str) -> int:
	return max(hike_lens(ip, True))