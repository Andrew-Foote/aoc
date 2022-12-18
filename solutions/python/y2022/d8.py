import functools as ft
import itertools as it
import operator
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW, Rect

test_inputs = [('example', '''\
30373
25512
65332
33549
35390\
''', [
	('interior_points_visible_csv', '1,1;2,1;1,2;3,2;2,3'),
	('p1', '21'),
	('p2', '8')
])]

def is_visible(grid: Grid[int], z0: gint) -> bool:
	rect = grid.rect()

	h = grid[z0]

	for d in NESW:
		for i in it.count(1):
			z = z0 + i * d

			if z not in rect:
				# visible in this direction, hence visible overall
				return True

			if grid[z] >= h:
				# not visible in this direction, might be visible in others
				break

	# ok, not visible in any direction
	return False

def parse_grid(ip: str) -> Grid[int]:
	return Grid([list(map(int, s)) for s in ip.splitlines()])

def interior_points_visible_csv(ip: str) -> bool:
	grid = parse_grid(ip)
	rect = grid.rect()
	interior = Rect(rect.top + 1, rect.right - 1, rect.bottom - 1, rect.left + 1)
	points = []

	for z in interior:
		if is_visible(grid, z):
			points.append(z)

	return ';'.join(f'{z.real},{z.imag}' for z in points)

def p1(ip: str) -> int:
	grid = parse_grid(ip)
	return sum(1 for z in grid.rect() if is_visible(grid, z))

def score(grid: Grid[int], z0: gint) -> int:
	rect = grid.rect()
	h = grid[z0]
	dists = {}

	for d in NESW:
		for i in it.count(1):
			z = z0 + i * d

			if z not in rect:
				dists[d] = i - 1
				break
			
			if grid[z] >= h:
				dists[d] = i
				break

	return ft.reduce(operator.mul, dists.values())

def p2(ip: str) -> int:
	grid = parse_grid(ip)
	return max(score(grid, z) for z in grid.rect())
