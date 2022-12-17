from dataclasses import dataclass
import functools as ft
import itertools as it
import re
from solutions.python.lib import graph

test_inputs = [('example', '''\
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\
''', [
	('p1', '3068'),
	('p2', '0')
])]

def rockgen():
	while True:
		yield ['####']

		yield [
			'.#.',
			'###',
			'.#.'
		]

		yield [
			'..#',
			'..#',
			'###'
		]

		yield [
			'#',
			'#',
			'#',
			'#'
		]

		yield [
			'##',
			'##'
		]

def parse(ip: str) -> None:
	while True:
		for c in ip.strip():
			yield c	

WIDTH = 7

def rockheight(rock):
	return len(rock)

def rock_coords(rock, rockpos):
	for i, row in enumerate(rock):
		for j, col in enumerate(row):
			if col == '#':
				yield rockpos[0] + j, rockpos[1] + i

def gridpic(grid):
	miny = min((0, *(y for x, y in grid.keys())))
	maxy = max((0, *(y for x, y in grid.keys())))
	lines = []

	for y in range(miny, maxy + 1):
		lines.append(''.join(grid.get((x, y), '.') for x in range(7)))

	print(grid)
	return '\n'.join(lines)

def runsim(rocks, jets):
	# this will yield the grid state when a rock finishes

	# this is the set of all coordinates at which there are rock tiles
	grid = {}


	while True:
		rock = next(rocks)
		print(f'this rock is falling: {rock}')
		#print(f'grid: {grid}')

		# the floor will be 0
		# coords above will be -

		rockbotgrounder = -get_tower_height(grid)
		rockpos = (2, rockbotgrounder - 3 - rockheight(rock))
		print(f'initial pos: {rockpos}')

		while True:
			# jet mvoe
			jet = next(jets)
			#print(f'jet: {jet}')

			if jet == '<':
				new_rockpos = (rockpos[0] - 1, rockpos[1])	
			elif jet == '>':
				new_rockpos = (rockpos[0] + 1, rockpos[1])
			else:
				assert False, jet

			# go over all the coords within the rock...
			for partpos in rock_coords(rock, new_rockpos):
				if not (0 <= partpos[0] < 7 and partpos[1] < 0 and partpos not in grid):
					# can't move
					#print('cannot move from jet')
					break
			else:
				#print(f'moved due to jet to {new_rockpos}')
				rockpos = new_rockpos

			# down move
			new_rockpos = (rockpos[0], rockpos[1] + 1)

			finished_falling = False

			# again go over all coords within the rock...
			for partpos in rock_coords(rock, new_rockpos):
				# check only for floor/rock contact first
				if partpos[1] >= 0 or partpos in grid:
					# rock has fallen
					finished_falling = True
					break
			else:
				#print(f'moved further down to {new_rockpos}')
				rockpos = new_rockpos

			if finished_falling:
				print(f'the rock landed at {rockpos}')
				for partpos in rock_coords(rock, rockpos):
					assert partpos not in grid
					grid[partpos] = '#'

				#print(gridpic(grid))
				#input()
				
				yield grid
				break

def get_tower_height(grid):
	if not grid:
		return 0

	return -min(y for x, y in grid.keys())

def p1(ip: str) -> int:
	rocks = rockgen()
	jet_pat = parse(ip)
	gridstates = runsim(rocks, jet_pat)
	list(it.islice(gridstates, 2021))
	grid = next(gridstates)
	print(gridpic(grid))
	return get_tower_height(grid)
	# 3225 is too high

def p2(ip: str) -> int:
	return 0