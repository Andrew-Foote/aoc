# this is too slow. why is it so slow
# because we were labelling the nodes with the depth before adding them to the visited set,
# that's why!

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

class LabelledVertex:
	__slots__ = ('value', 'label')
	def __init__(self, value, label):
		self.value = value
		self.label = label

	def __eq__(self, other):
		return self.value == other.value

	def __hash__(self):
		return hash(self.value)

def depth_labelled(start, neighbours):
	def depth_labelled_neighbours(depth_labelled_vertex):
		#print(f'{depth_labelled_vertex=}')

		for neighbour in neighbours(depth_labelled_vertex.value):
			#print(f'(depth_labelled) {neighbour=}')
			yield LabelledVertex(neighbour, depth_labelled_vertex.label + 1)

	return LabelledVertex(start, 0), depth_labelled_neighbours

def depth_limited(start, neighbours, limit):
	start, neighbours = depth_labelled(start, neighbours)

	def depth_limited_neighbours(depth_labelled_vertex):
		#print(f'depth_limited_neighbours.{depth_labelled_vertex=}')

		if depth_labelled_vertex.label < limit:
			yield from neighbours(depth_labelled_vertex.value)

	return start, depth_limited_neighbours

#@profile
def gbfs(start, neighbours):
	visited = {start}
	queue = deque([start])

	while queue:
		#print(len(queue))
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

		for depth_labelled_vertex in gdfs(depth_labelled_start, depth_limited_neighbours):
			if depth_labelled_vertex.label == limit:
				yield depth_labelled_vertex
				level_size += 1

		#print(f'{level_size=}')
		if not level_size:
			break

#GRID_DIRS4 = tuple(map(np.array, ((-1, 0), (1, 0), (0, -1), (0, 1))))
GRID_DIRS4 = ((-1, 0), (1, 0), (0, -1), (0, 1))

#@profile
def grid_neighbours4(grid, pos):
	for offset in GRID_DIRS4:
		#neighbour = pos + offset
		neighbour = pos[0] + offset[0], pos[1] + offset[1]

		#if np.all(0 <= neighbour) and np.all(neighbour < grid.shape):
		if 0 <= neighbour[0] < grid.shape[0] and 0 <= neighbour[1] < grid.shape[1]:
			yield neighbour

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

#@profile
def p1(ip):
	area, start, end = parse(ip)
	#print(f'{area=}')
	#print(f'{start=}')
	#print(f'{end=}')

	def neighbours(pos):
		#print(f'neighbours.{pos=}')
		for neighbour in grid_neighbours4(area, pos):
			if can_step_to(area, pos, neighbour):
				#print('  can step to passed')
				yield neighbour

	start, neighbours = depth_labelled(start, neighbours)

	for pos in gbfs(start, neighbours):
		if pos.value == end:
			return pos.label

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

	for pos in gbfs(start, neighbours):
		if pos.value == end:
			return pos.label - 1

	assert False

if __name__ == '__main__':
	# with open('input/2022/12.txt') as f:
	# 	ip = f.read()

	ip = '''\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi\
'''

	print(p1(ip))


