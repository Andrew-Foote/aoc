from collections import deque
from typing import Iterator

test_inputs = [('example', '''\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi\
''', [
	('p1', '31')
])]

Vec2D = tuple[int, int]
Matrix = tuple[tuple[int, ...]]

def vadd(u: Vec2D, v: Vec2D) -> Vec2D:
	i1, j1 = u
	i2, j2 = v
	#print(u, v)
	return i1 + i2, j1 + j2

def mentry(m: Matrix, v: Vec2D) -> int:
	i, j = v
	return m[i][j]

def mnrows(m: Matrix) -> int: return len(m)
def mncols(m: Matrix) -> int: return len(m[0])

DIRS = ((-1, 0), (1, 0), (0, -1), (0, 1))

def neighbours(mat: Matrix, idx: Vec2D) -> Iterator[Vec2D]:
	for dir_ in DIRS:
		nb = vadd(idx, dir_)

		if (
			0 <= nb[0] < mnrows(mat) and 0 <= nb[1] < mncols(mat)
			and mentry(mat, nb) <= mentry(mat, idx) + 1
		):
			yield vadd(idx, dir_)

def bfs(mat: Matrix, start: Vec2D) -> Iterator[Vec2D]:
	queue = deque([(0, start)])
	visited = set()

	while queue:
		dist, cur = queue.pop()
		print((dist, cur), end=', ')
		visited.add(cur)
		#print('yielding dist', dist, 'cur', cur)
		yield dist, cur
		nbs = list(neighbours(mat, cur))
		#print('neighbours', nbs)
		#print('appending: ', tuple((dist + 1, nb) for nb in nbs))
		queue.extendleft((dist + 1, nb) for nb in nbs if nb not in visited)

def dfs(mat: Matrix, start: Vec2D) -> Iterator[Vec2D]:
	stack = [(0, start)]
	visited = set()

	while stack:
		dist, cur = stack.pop()
		visited.add(cur)
		#print('yielding dist', dist, 'cur', cur)
		yield dist, cur
		nbs = list(neighbours(mat, cur))
		#print('neighbours', nbs)
		#print('appending: ', tuple((dist + 1, nb) for nb in nbs))
		stack.extend((dist + 1, nb) for nb in nbs if nb not in visited)

def iddfs(mat: Matrix, start: Vec2D) -> Iterator[Vec2D]:
	level = 0

	while True:
		level_is_empty = True
		stack = [(0, start)]
		put_in_stack = {start}

		while stack:
			dist, cur = stack.pop()

			if dist == level:			
				#print((dist, cur), end = ', ')
				level_is_empty = False
				yield dist, cur
			else:
				nbs = list(neighbours(mat, cur))
				stack.extend((dist + 1, nb) for nb in nbs if nb not in put_in_stack)
				for nb in nbs:
					put_in_stack.add(nb)

		level += 1
		if level_is_empty: break

def parse(ip: str) -> tuple[Matrix, Vec2D, Vec2D]:
	rows = []
	start = None
	end = None

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

		rows.append(tuple(row))

	assert start is not None
	assert end is not None
	return tuple(rows), start, end

def p1(ip: str) -> int:
	mat, start, end = parse(ip)
	#print('beginmat')
	#print(mat)
	#print('endmat')
	#print('start', start)
	#print('end', end)

	for dist, idx in iddfs(mat, start):
		#print('dist', dist)
		#print('idx', idx)
		if idx == end:
			return dist
	else:
		raise ValueError('didnt find et')