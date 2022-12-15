from collections import defaultdict
from enum import Enum
import itertools as it
from typing import Callable, Iterator
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

def grid_pic(rect: Rect, grid: Callable[[complex], Tile]) -> str:	
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
			chars.append(grid(complex(x, y)).value)

		prefix = str(y) + ' ' * (len(digits(rect.bottom)) - len(str(y)) + 1)
		lines.append(prefix + ''.join(chars))

	# print()
	# print('\n'.join(lines))
	# print()
	return '\n'.join(lines)

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

def pic(ip: str) -> str:
	grid = parse_grid(ip)
	return grid_pic(grid.rect(), lambda z: grid[z])

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
				# print(grid_pic(grid.rect(), lambda z: grid[z]))
				# print()
				# input()
				break

def p2_parse_grid(ip: str) -> DefaultGrid[Tile]:
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

	return grid

def p2(ip: str) -> int:
	grid = p2_parse_grid(ip)

	for i in it.count():
		sand_pos = SAND_SOURCE_POS

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
				# print(grid_pic(grid.rect(), lambda z: grid[z]))
				# print()
				# input()
				break

def run_grid(grid: Grid[Tile]) -> Iterator[Grid[Tile]]:
	for i in it.count():
		sand_pos = SAND_SOURCE_POS
		falling = True

		while falling:
			break_reason = None

			for d in SAND_FALL_DIRS:
				new_pos = sand_pos + d

				if new_pos.imag > rect.bottom:
					grid[sand_pos] = Tile.AIR
					falling = False
					break

				if grid[new_pos] == Tile.AIR:
					if grid[sand_pos] == Tile.FALLING_SAND:
						grid[sand_pos] = Tile.AIR

					grid[new_pos] = Tile.FALLING_SAND
					yield grid
					sand_pos = new_pos
					break
			else:
				grid[sand_pos] = Tile.RESTING_SAND
				falling = False
				#yield grid
				# print()
				# print(grid_pic(grid.rect(), lambda z: grid[z]))
				# print()
				# input()

if __name__ == '__main__':
	import sys

	with open('input/2022/14.txt') as f:
		ip = f.read()

	grid = parse_grid(ip)
	rect = grid.rect()
	print(rect.width, rect.height)

	import numpy as np
	import sdl2
	import sdl2.ext
	import sdl2.ext.pixelaccess

	sdl2.ext.init()
	window = sdl2.ext.Window('Advent of Code Day 14 Animation', size=(rect.width * 5, rect.height * 5))

	surface = window.get_surface()
	view = sdl2.ext.pixelaccess.pixels2d(surface)

	TILE_COLOR = {
		Tile.AIR: 0x00_00_00,
		Tile.ROCK: 0x80_80_80,
		Tile.SAND_SOURCE: 0xff_ff_ff,
		Tile.FALLING_SAND: 0xff_ff_00,
		Tile.RESTING_SAND: 0xff_00_00
	}

	def grid_as_array():
		return np.block([
			[
				np.full((5, 5), TILE_COLOR[grid[complex(x, y)]], dtype='uint32')
				for y in range(rect.top, rect.bottom + 1)
			]
			for x in range(rect.left, rect.right + 1)
		])

	print(grid_as_array().shape)

	def do_update():
		np.copyto(view, grid_as_array())

	do_update()

	window.show()

	last_update_ticks = sdl2.SDL_GetTicks()
	ticks = None
	iterator = run_grid(grid)
	exhausted = False

	TICKS_PER_UPDATE = 10
	STEPS_PER_UPDATE = 250

	while True:
		events = sdl2.ext.get_events()

		for event in events:
			if event.type == sdl2.SDL_QUIT:
				sys.exit()

		if not exhausted:
			ticks = sdl2.SDL_GetTicks()

			if ticks - last_update_ticks > TICKS_PER_UPDATE:
				for _ in range(STEPS_PER_UPDATE):
					try:
						grid = next(iterator)
					except StopIteration:
						exhausted = True
						break

				do_update()
				last_update_ticks = ticks

		window.refresh()