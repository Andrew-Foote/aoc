import itertools as it
from solutions.python.lib.gint import gint
import solutions.python.lib.grid as g

test_inputs = [
	(
		'example', '''\
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....''',
		[
			('p1', 374)
		]
	)
]

def taxicab(p1: gint, p2: gint) -> int:
	return abs(p1.real - p2.real) + abs(p1.imag - p2.imag)

def parse(ip: str, p2: bool=False) -> list[gint]:
	grid = g.Grid(ip.splitlines())
	empty_rows = [i for i in range(grid.height) if all(char == '.' for char in grid.rows[i])]
	empty_cols = [j for j in range(grid.width) if all(char == '.' for char in grid.cols[j])]
	galaxies = []

	for y, line in enumerate(ip.splitlines()):
		for x, char in enumerate(line):
			if char == '#':
				galaxies.append(gint(x, y))

	m = 1000000 - 1 if p2 else 1

	for k, galaxy in enumerate(galaxies):
		galaxies[k] = gint(
			sum(1 + m * (j in empty_cols) for j in range(galaxy.real)),
			sum(1 + m * (i in empty_rows) for i in range(galaxy.imag))
		)

	return galaxies

def p1(ip: str) -> int:
	galaxies = parse(ip)
	return sum(taxicab(g1, g2) for g1, g2 in it.combinations(galaxies, 2))

def p2(ip: str) -> int:
	galaxies = parse(ip, True)
	return sum(taxicab(g1, g2) for g1, g2 in it.combinations(galaxies, 2))
