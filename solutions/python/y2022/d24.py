from collections import Counter
import functools as ft
from typing import Iterator, Optional
from solutions.python.lib.gint import gint
from solutions.python.lib import graph
from solutions.python.lib.grid import Grid, NESW

test_inputs = [('example', '''\
#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#\
''', [
	('p1', 18),
	('p2', 54)
])]

WALL = ('#',)
GROUND = ('.',)

BLIZZARDS = {
	'>': gint(1, 0),
	'<': gint(-1, 0),
	'^': gint(0, -1),
	'v': gint(0, 1)
}

def parse(ip: str) -> Grid[Counter[gint]]:
	lines = [line[1:-1] for line in ip.splitlines()[1:-1]]
	
	return Grid(
		[
			(Counter() if char == '.' else Counter([BLIZZARDS[char]]))
			for char in line
		]
		for line in lines
	)

def move_blizzards(grid: Grid[Counter[gint]]) -> Grid[Counter[gint]]:
	rect = grid.rect()
	new_grid: Grid[Counter[gint]] = Grid.fromrect(rect, lambda _: Counter())

	for pos in rect:
		for blizzard, count in grid[pos].items():
			new_pos = pos + blizzard
			new_pos = gint(new_pos.real % rect.width, new_pos.imag % rect.height)
			new_grid[new_pos][blizzard] += 1

	#print(grid_pic(new_grid))
	return new_grid

def grid_pic(grid: Grid[Counter[gint]], playerpos: Optional[gint]=None) -> str:
	rect = grid.rect()

	def draw(z: gint) -> str:
		if playerpos is not None and z == playerpos:
			assert not grid[z], grid[z]
			return 'E'

		if not grid[z]:
			return '.'

		blizzards = grid[z]
		count = sum(k for _, k in blizzards.items())

		if count == 1:
			direction = list(blizzards)[0]
			blizzard = next(blizzard for blizzard, bdir in BLIZZARDS.items() if bdir == direction)
			return blizzard

		return str(count)

	return '\n' + rect.picture(draw) + '\n'

def available_moves(pos: gint, grid: Grid[Counter[gint]]) -> Iterator[gint]:
	rect = grid.rect()

	if pos not in rect or not grid[pos]:
		yield pos	

	for direction in NESW:
		new_pos = pos + direction
		# print(f'considering {new_pos} in direction {direction}')
		# if new_pos in rect: print(f'grid here is {grid[new_pos]}')
		# else: print('not in grid so not yielding')

		if new_pos in (gint(0, -1), gint(rect.width - 1, rect.height)) or (new_pos in rect and not grid[new_pos]):
			yield new_pos

def p1(ip: str) -> int:
	grid = parse(ip)
	#print(grid_pic(grid))
	rect = grid.rect()
	pos = gint(0, -1)

	@ft.cache
	def grid_at_step(steps: int) -> Grid[Counter[gint]]:
		print(f'step {steps}')

		if steps == 0:
			return grid
		return move_blizzards(grid_at_step(steps - 1))

	def children(node: tuple[int, gint, list[gint]]) -> Iterator[tuple[int, gint]]:
		steps, pos, path = node
		moves = list(available_moves(pos, grid_at_step(steps + 1)))

		for new_pos in moves:
			yield steps + 1, new_pos, () #(*path, new_pos)

	for steps, pos, path in graph.gbfs((0, pos, (pos,)), children):
		# print(steps, pos, path)
		if pos == rect.bottom_right:
			#assert len(path) == steps + 1
			# print(path)

			for i, prevpos in enumerate(path):
				# print()
				# print(prevpos)
				prevgrid = grid_at_step(i)
				# print(grid_pic(prevgrid, prevpos))

			return steps + 1

	assert False, 'search finished'
	# 220 too low

def p2(ip: str) -> int:
	grid = parse(ip)
	rect = grid.rect()
	pos = gint(0, -1)

	@ft.cache
	def grid_at_step(steps: int) -> Grid[Counter[gint]]:
		print(f'step {steps}')

		if steps == 0:
			return grid
		return move_blizzards(grid_at_step(steps - 1))

	def children(node: tuple[int, gint, int]) -> Iterator[tuple[int, gint, int]]:
		steps, pos, phase = node

		if phase == 0 and pos == rect.bottom_right + gint(0, 1):
			phase += 1
		elif phase == 1 and pos == rect.top_left + gint(0, -1):
			phase += 1

		moves = list(available_moves(pos, grid_at_step(steps + 1)))

		for new_pos in moves:
			yield steps + 1, new_pos, phase

	for steps, pos, phase in graph.gbfs((0, pos, 0), children):
		if phase == 2 and pos == rect.bottom_right + gint(0, 1):
			return steps

	assert False, 'search finished'
	# 220 too low