from scipy import sparse
import numpy as np
from solutions.python.lib.gint import gint
import solutions.python.lib.grid as g

test_inputs = [
	('example', '''\
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........''', [
		('one_step', '''\
...........
.....###.#.
.###.##..#.
..#.#...#..
....#O#....
.##.OS####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........'''),
		('two_step', '''\
...........
.....###.#.
.###.##..#.
..#.#O..#..
....#.#....
.##O.O####.
.##.O#...#.
.......##..
.##.#.####.
.##..##.##.
...........'''),
		('three_step', '''\
...........
.....###.#.
.###.##..#.
..#.#.O.#..
...O#O#....
.##.OS####.
.##O.#...#.
....O..##..
.##.#.####.
.##..##.##.
...........'''),
		('six_step', '''\
...........
.....###.#.
.###.##.O#.
.O#O#O.O#..
O.O.#.#.O..
.##O.O####.
.##.O#O..#.
.O.O.O.##..
.##.#.####.
.##O.##.##.
...........'''),
		('six_step_count', 16),
		('p1', 23),
		('ten_step_count', 50),
		('fifty_step_count', 1594),
		('hundred_step_count', 6536),
		('five_hundred_step_count', 167004),
		('thousand_step_count', 668697),
		('five_thousand_step_count', 16733044)
	])
]

# S = start
# . or S = garden plots
# # = rocks
# elf can only move through plots
# he needs to get 64 steps - q is how many different plots he could
# potentially arrive at at the end of those steps?

# in other words what is the set of endpoints of all 64-step paths through the graph

def parse_to_grid(ip: str) -> g.Grid:
	return g.Grid(ip.splitlines())

def parse_to_graph(grid: g.Grid) -> tuple[set[tuple[gint, gint]], gint]:
	graph = set()

	for z in grid.rect():
		if grid[z] == '#':
			continue
		elif grid[z] == 'S':
			start = z

		for d in g.NESW:
			if z + d in grid.rect() and grid[z + d] != '#':
				graph.add((z, z + d))

	return graph, start
	# we want the row/col of the adjmat containing the S point, and we want to know how many entries are nonzero

def node_list(graph: set[tuple[gint, gint]]) -> list[gint]:
	return sorted({e[0] for e in graph} | {e[1] for e in graph}, key=lambda z: (z.real, z.imag))

def adj_matrix(graph: set[tuple[gint, gint]], start: gint) -> tuple[sparse.csr_array, int]:
	nodes = node_list(graph)
	print('NODES COUNT: ', len(nodes))
	
	row = []
	col = []
	data = []

	node_to_index_map = {n: i for i, n in enumerate(nodes)}
	start_index = node_to_index_map[start]

	for i, (n1, n2) in enumerate(graph):
		row.append(node_to_index_map[n1])
		col.append(node_to_index_map[n2])
		data.append(1)

	m = sparse.csr_array(
		(data, (row, col)), shape=(len(nodes),) * 2, dtype=np.ubyte
	)

	return m, start_index

def step_pic(ip: str, count: int) -> str:
	grid = parse_to_grid(ip)
	graph, start = parse_to_graph(grid)
	nodes = node_list(graph)
	point_to_index_map = {z: i for i, z in enumerate(nodes)}
	m, start_index = adj_matrix(graph, start)
	m = sparse.linalg.matrix_power(m, count)

	def picmaker(z: gint) -> str:
		if z not in point_to_index_map:
			assert grid[z] == '#', z
			return '#'
		elif m[start_index, point_to_index_map[z]] > 0:
			assert grid[z] in '.S'
			return 'O'
		elif z == start:
			return 'S'
		else:
			assert grid[z] == '.'
			return '.'

	p = grid.rect().picture(picmaker)
	print()
	print(p)
	print()
	return p

def step_count(ip: str, count: int) -> int:
	grid = parse_to_grid(ip)
	graph, start = parse_to_graph(grid)
	nodes = node_list(graph)
	point_to_index_map = {z: i for i, z in enumerate(nodes)}
	m, start_index = adj_matrix(graph, start)
	m = step(m, count)
	return sum(1 for j, _ in enumerate(nodes) if m[start_index, j] > 0)

def one_step(ip: str) -> str: return step_pic(ip, 1)
def two_step(ip: str) -> str: return step_pic(ip, 2)
def three_step(ip: str) -> str: return step_pic(ip, 3)
def six_step(ip: str) -> str: return step_pic(ip, 6)
def six_step_count(ip: str) -> int: return step_count(ip, 6)

def p1(ip: str) -> int:
	return step_count(ip, 64)

def p2(ip: str) -> int:
	base_grid = parse_to_grid(ip)

	grid = g.DefaultGrid(lambda z: base_grid[
		gint(z.real % base_grid.width, z.imag % base_grid.height)
	], base_grid)


	# basically it's a "chessboard" pattern
	# given a number of steps, we may consider the subgrid