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
	('p1', '31'),
	('p2', '29')
])]

def depth_labelled(start, neighbours):
	def depth_labelled_neighbours(depth_labelled_vertex):
		#print(f'{depth_labelled_vertex=}')
		depth, vertex = depth_labelled_vertex

		for neighbour in neighbours(vertex):
			#print(f'(depth_labelled) {neighbour=}')
			yield depth + 1, neighbour

	return (0, start), depth_labelled_neighbours

def depth_limited(start, neighbours, limit):
	start, neighbours = depth_labelled(start, neighbours)

	def depth_limited_neighbours(depth_labelled_vertex):
		depth, vertex = depth_labelled_vertex
		#print(f'depth_limited_neighbours.{depth_labelled_vertex=}')

		if depth < limit:
			yield from neighbours(depth_labelled_vertex)

	return start, depth_limited_neighbours

def gbfs(start, neighbours):
	visited = {start}
	queue = deque([start])
	i = 0

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

def gdfs(start, neighbours):
	visited = {start}
	yield start
	stack = [iter(neighbours(start))]

	while stack:
		iterneighbours = stack[-1]

		try:
			node = next(iterneighbours)
		except StopIteration:
			del stack[-1]
		else:
			#print(f'gdfs.{node=}')

			if node not in visited:
				yield node
				visited.add(node)
				stack.append(iter(neighbours(node)))

def giddfs(start, neighbours):
	for limit in it.count():
		#print(f'giddfs.{limit=}')
		depth_labelled_start, depth_limited_neighbours = depth_limited(start, neighbours, limit)
		#print(f'giddfs.{depth_labelled_start=}')
		level_size = 0

		for depth, vertex in gdfs(depth_labelled_start, depth_limited_neighbours):
			if depth == limit:
				yield depth, vertex
				level_size += 1

		print(f'{level_size=}')
		if not level_size:
			break

GRID_DIRS4 = tuple(map(np.array, ((-1, 0), (1, 0), (0, -1), (0, 1))))

def grid_neighbours4(grid, pos):
	for offset in GRID_DIRS4:
		neighbour = pos + offset

		if np.all(0 <= neighbour) and np.all(neighbour < grid.shape):
			yield tuple(neighbour)

def can_step_to(area, from_pos, to_pos):
	#print(f'(can_step_to) {from_pos=}, {to_pos=}, {grid[to_pos]=}, {grid[from_pos]=}')
	return area[to_pos] <= area[from_pos] + 1

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

def p2(ip):
	area, start, end = parse(ip)
	starts = [start] + [pos for pos, ele in np.ndenumerate(area) if not ele]

	def neighbours(pos):
		if pos is None:
			yield from starts
			return

		for neighbour in grid_neighbours4(area, pos):
			if can_step_to(area, pos, neighbour):
				#print('  can step to passed')
				yield neighbour

	start, neighbours = depth_labelled(None, neighbours)

	for depth, coords in gbfs(start, neighbours):
		if coords == end:
			return depth - 1

	assert False

