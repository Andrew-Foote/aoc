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
		('p1', 94)
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

def accessible_neighbours(grid: g.Grid, point: gint) -> Iterator[gint]:
	v = grid[point]

	if v in SLOPES:
		neighbours = [point + SLOPES[v]]
	else:
		neighbours = [point + d for d in g.NESW]

	for n in neighbours:
		if grid[n] != '#':
			yield n

def possible_path_extensions(grid: g.Grid, path: list[gint]) -> Iterator[list[gint]]:
	for neighbour in accessible_neighbours(grid, path[-1]):
		if neighbour not in path:
			yield path + [neighbour]

def find_start(grid: g.Grid):
	for i in range(grid.width):
		if grid[gint(i, 0)] == '.':
			return gint(i, 0)

def find_end(grid: g.Grid):
	for i in range(grid.width):
		if grid[gint(i, grid.height - 1)] == '.':
			return gint(i, grid.height - 1)

def hikes(grid: g.Grid) -> list[list[gint]]:
	start = find_start(grid)
	end = find_end(grid)

	paths = [[start]]
	successful_paths = []

	while paths:
		new_paths = []

		for path in paths:
			if path[-1] == end:
				successful_paths.append(path)
			else:
				new_paths.extend(possible_path_extensions(grid, path))

		print(len(new_paths), len(successful_paths))
		paths = new_paths

	return successful_paths

def hike_lens(ip: str) -> list[int]:
	return sorted(len(hike) - 1 for hike in hikes(parse(ip)))

def hike_lens_csv(ip: str) -> str:
	return ','.join(map(str, hike_lens(ip)))

def p1(ip: str) -> int:
	return max(hike_lens(ip))