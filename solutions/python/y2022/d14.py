from collections import defaultdict
from enum import Enum
import itertools as it
from typing import Iterator
from solutions.python.lib.grid import DefaultGrid, Grid, Path, Rect
from solutions.python.lib.digits import digits

test_inputs = [('example', '''\
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9\
''', [
	('pic', '''\
  4     5  5
  9     0  0
  4     0  3
0 ......+...
1 ..........
2 ..........
3 ..........
4 ....#...##
5 ....#...#.
6 ..###...#.
7 ........#.
8 ........#.
9 #########.\
'''),
	('p1', '24'),
	('p2', '93')
])]

def parse_paths(ip: str) -> Iterator[Path]:
	for line in ip.splitlines():
		point_descs = line.split(' -> ')
		points = []

		for point_desc in point_descs:
			x, y = map(float, point_desc.split(','))
			points.append(complex(x, y))

		yield Path(points)

SAND_SOURCE_POS = 500 + 0j

class Tile(Enum):
	AIR = '.'
	ROCK = '#'
	SAND_SOURCE = '+'
	FALLING_SAND = '~'
	RESTING_SAND = 'o'

def parse_grid(ip: str) -> Grid[Tile]:
	paths = list(parse_paths(ip))
	
	rect = Rect.bounding(it.chain(
		[SAND_SOURCE_POS],
		it.chain.from_iterable(path.points for path in paths)
	))

	full_paths = [list(path) for path in paths]
	rock_locs = set(it.chain.from_iterable(full_paths))

	def tile(z):
		if z in rock_locs:
			return Tile.ROCK
		elif z == SAND_SOURCE_POS:
			return Tile.SAND_SOURCE
		else:
			return Tile.AIR

	return Grid.fromrect(rect, tile)

def grid_pic(grid: Grid[Tile]) -> str:	
	rect = grid.rect()
	lines = []

	for place in range(len(digits(rect.right)) - 1, -1, -1):
		chars = (
			[str(digits(rect.left).get(place, ' '))]
			+ [' '] * (rect.width - 2)
			+ [str(digits(rect.right)[place])]
		)

		chars[int(SAND_SOURCE_POS.real) - rect.left] = str(digits(int(SAND_SOURCE_POS.real))[place])
		prefix = ' ' * (len(digits(rect.bottom)) + 1)
		lines.append(prefix + ''.join(chars))

	for y in range(rect.top, rect.bottom + 1):
		chars = []

		for x in range(rect.left, rect.right + 1):
			chars.append(grid[complex(x, y)].value)

		prefix = str(y) + ' ' * (len(digits(rect.bottom)) - len(str(y)) + 1)
		lines.append(prefix + ''.join(chars))

	# print()
	# print('\n'.join(lines))
	# print()
	return '\n'.join(lines)

def pic(ip: str) -> str:
	return grid_pic(parse_grid(ip))

SAND_FALL_DIRS = (1j, -1 + 1j, 1 + 1j)

def p1(ip: str) -> int:
	grid = parse_grid(ip)
	rect = grid.rect()

	for i in it.count():
		sand_pos = SAND_SOURCE_POS

		while True:
			for d in SAND_FALL_DIRS:
				new_pos = sand_pos + d

				if new_pos.imag > rect.bottom:
					# print('reached bottom, returning')
					return i

				# if new_pos not in rect:
				# 	print(f'{new_pos} out of bounds')
				# 	continue

				if grid[new_pos] == Tile.AIR:
					# print(f'can move to {new_pos}')

					if grid[sand_pos] == Tile.FALLING_SAND:
						# print(f'setting {sand_pos} back to air')
						grid[sand_pos] = Tile.AIR

					grid[new_pos] = Tile.FALLING_SAND
					sand_pos = new_pos
					break
			else:
				grid[sand_pos] = Tile.RESTING_SAND
				# print()
				# print(grid_pic(grid))
				# print()
				# input()
				break

def grid_pic2(grid: DefaultGrid[Tile]) -> str:	
	rect = grid.rect()
	lines = []

	for place in range(len(digits(rect.right)) - 1, -1, -1):
		chars = (
			[str(digits(rect.left).get(place, ' '))]
			+ [' '] * (rect.width - 2)
			+ [str(digits(rect.right)[place])]
		)

		chars[int(SAND_SOURCE_POS.real) - rect.left] = str(digits(int(SAND_SOURCE_POS.real))[place])
		prefix = ' ' * (len(digits(rect.bottom)) + 1)
		lines.append(prefix + ''.join(chars))

	for y in range(rect.top, rect.bottom + 1):
		chars = []

		for x in range(rect.left, rect.right + 1):
			chars.append(grid[complex(x, y)].value)

		prefix = str(y) + ' ' * (len(digits(rect.bottom)) - len(str(y)) + 1)
		lines.append(prefix + ''.join(chars))

	# print()
	# print('\n'.join(lines))
	# print()
	return '\n'.join(lines)

def p2(ip: str) -> int:
	paths = list(parse_paths(ip))
	rocks = set(it.chain.from_iterable(paths))
	floor = Rect.bounding((SAND_SOURCE_POS, *rocks)).bottom + 2

	def base_grid(z: complex) -> Tile:
		if z.imag == floor:
			return Tile.ROCK
		else:
			return Tile.AIR

	grid = DefaultGrid(base_grid)
	grid[SAND_SOURCE_POS] = Tile.SAND_SOURCE

	for path in paths:
		for point in path:
			grid[point] = Tile.ROCK

	for i in it.count():
		sand_pos = 500 + 0j

		while True:
			for d in SAND_FALL_DIRS:
				new_pos = sand_pos + d

				if grid[new_pos] == Tile.AIR:
					if grid[sand_pos] == Tile.FALLING_SAND:
						grid[sand_pos] = Tile.AIR

					grid[new_pos] = Tile.FALLING_SAND
					sand_pos = new_pos
					break
			else:
				if sand_pos == SAND_SOURCE_POS:
					return i + 1

				grid[sand_pos] = Tile.RESTING_SAND
				# print()
				# print(grid_pic2(grid))
				# print()
				# input()
				break