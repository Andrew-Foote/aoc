from dataclasses import dataclass
import functools as ft
import itertools as it
import re
from solutions.python.lib import graph

test_inputs = [('example', '''\
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\
''', [
	('p1', '3068'),
	('p2', '1514285714288')
])]

ROCKS = [
	['####'],
	[
		'.#.',
		'###',
		'.#.'
	],
	[
		'..#',
		'..#',
		'###'
	],
	[
		'#',
		'#',
		'#',
		'#'
	],
	[
		'##',
		'##'
	]
]

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

def parsejets(ip: str) -> list[str]:
	return list(ip.strip())	

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

	if grid: maxy = max((y for x, y in grid.keys()))
	else: maxy = 0

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
		#print(f'this rock is falling: {rock}')
		#print(f'grid: {grid}')

		# the floor will be 0
		# coords above will be -

		rockbotgrounder = -get_tower_height(grid)
		rockpos = (2, rockbotgrounder - 3 - rockheight(rock))
		
		#print(f'initial pos: {rockpos}')

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
				#print(f'the rock landed at {rockpos}')
				for partpos in rock_coords(rock, rockpos):
					assert partpos not in grid
					grid[partpos] = '#'

				# we can purge all bits from the grid that are no longer relevant
				# for each x from 0 to 6, find the min y occupied by a rock.
				# we can ignore any parts of the grid below the max of all those ys.

				ymins = [min((0, *(
					y for x, y in grid.keys()
					if x == x0 and grid[x, y] == '#'
				))) for x0 in range(7)]

				ylim = max(ymins)
				keystoremove = [(x, y) for x, y in grid.keys() if y > ylim]

				for k in keystoremove:
					del grid[k]

				# print(gridpic(grid))
				# input()

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
	#print(gridpic(grid))
	return get_tower_height(grid)
	# 3225 is too high

def rockgen1():
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

def parse1(ip: str) -> None:
	for c in ip.strip():
		yield c	

def p1lim(ip: str, lim: int) -> int:
	rocks = rockgen()
	jet_pat = parse(ip)
	gridstates = runsim(rocks, jet_pat)
	list(it.islice(gridstates, lim))
	grid = next(gridstates)
	return get_tower_height(grid)

def normgridstate(gs):
	y0 = max(y for x, y in gs.keys())

	return {
		(x, y - y0): v
		for (x, y), v
		in gs.items()
	}

def p2(ip: str) -> int:
	# the pattern will repeat eventually?	
	jetlist = parsejets(ip)

	rock_i = 0
	jet_i = 0

	def rockgen_here():
		nonlocal rock_i

		while True:
			rock = ROCKS[rock_i]
			rock_i += 1

			if rock_i >= len(ROCKS):
				rock_i = 0

			yield rock

	def jetgen_here():
		nonlocal jet_i

		while True:
			jet = jetlist[jet_i]
			jet_i += 1

			if jet_i >= len(jetlist):
				jet_i = 0

			yield jet

	rocks4ever = rockgen_here()
	jets4ever = jetgen_here()
	gridstates = runsim(rocks4ever, jets4ever)
	mstates = {}

	for i in it.count():
		grid = next(gridstates)
		height = get_tower_height(grid)

		mstate = (
			rock_i, jet_i,
			tuple(sorted(normgridstate(grid).items()))
		)

		if mstate in mstates:
			# we found the period!
			prev_i, height_before_period = mstates[mstate]
			states_before_period = prev_i
			period = i - prev_i
			period_height_inc = height - height_before_period
			break
		else:
			mstates[mstate] = (i, height)

	# so height will be height_before_period + period_height_inc * T
	# where T is how many periods we run it for

	desired_generations = 1_000_000_000_000
	desired_gens_after_period = desired_generations - states_before_period
	q, r = divmod(desired_gens_after_period, period)

	vv = height_before_period + q * period_height_inc 

	curheight = height_before_period + period_height_inc

	if r:
		list(it.islice(gridstates, r - 1))
		grid = next(gridstates)
		rheight = get_tower_height(grid)

		vv += rheight - curheight
	return vv - 1
