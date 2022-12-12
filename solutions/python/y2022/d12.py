from collections import deque
import functools as ft
import itertools as it
from typing import Iterator
import numpy as np

test_inputs = [('example', '''\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi\
''', [
	('path', '''\
v..v<<<<
>v.vv<<^
.>vv>E^^
..v>>>^^
..>>>>>^'''),
	('p1', '31')
])]

def depth_labelled(start, neighbours):
	def depth_labelled_neighbours(depth_labelled_vertex):
		#print(f'{depth_labelled_vertex=}')
		depth, vertex = depth_labelled_vertex

		for neighbour in neighbours(vertex):
			#print(f'(depth_labelled) {neighbour=}')
			yield depth + 1, neighbour

	return (0, start), depth_labelled_neighbours

def gbfs(start, neighbours):
	visited = {start}
	queue = deque([start])

	while queue:
		print(len(queue))
		#print(f'{queue=}')
		node = queue.pop()
		#print(f'{node=}')
		yield node

		for neighbour in neighbours(node):
			#print(f'  {neighbour=}')
			if neighbour not in visited:
				visited.add(neighbour)
				queue.appendleft(neighbour)

GRID_DIRS4 = tuple(map(np.array, ((-1, 0), (1, 0), (0, -1), (0, 1))))

def grid_neighbours4(grid, pos):
	for offset in GRID_DIRS4:
		neighbour = pos + offset

		if np.all(0 <= neighbour) and np.all(neighbour < grid.shape):
			yield tuple(neighbour)

def can_step_to(grid, from_pos, to_pos):
	#print(f'(can_step_to) {from_pos=}, {to_pos=}, {grid[to_pos]=}, {grid[from_pos]=}')
	return grid[to_pos] <= grid[from_pos] + 1

def parse(ip):
	rows = []

	for i, line in enumerate(ip.splitlines()):
		row = []

		for j, c in enumerate(line):
			if c == 'S':
				start = i, j
				c = 'a'
			elif c == 'E':
				end = i, j
				c = 'z'

			row.append(ord(c) - ord('a'))

		rows.append(row)

	return np.array(rows), start, end

def p1(ip):
	area, start, end = parse(ip)
	#print(f'{area=}')
	#print(f'{start=}')
	#print(f'{end=}')

	def neighbours(pos):
		for neighbour in grid_neighbours4(area, pos):
			if can_step_to(area, pos, neighbour):
				#print('  can step to passed')
				yield neighbour

	start, neighbours = depth_labelled(start, neighbours)

	for depth, coords in gbfs(start, neighbours):
		if coords == end:
			return depth

	assert False