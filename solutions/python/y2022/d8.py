test_inputs = [('example', '''\
30373
25512
65332
33549
35390\
''', [
	('p1', '21')
])]

def p1(ip: str) -> int:
	grid = [list(map(int, s)) for s in ip.splitlines()]
	h = len(grid)
	w = len(grid[0])
	assert all(len(row) == w for row in grid[1:])
	trees = [(i, j) for i in range(h) for j in range(w)]

	def is_visible(i: int, j: int) -> bool:
		treeheight = grid[i][j]

		for d in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
			new_i = i
			new_j = j

			while True:
				new_i = new_i + d[0]
				new_j = new_j + d[1]
				
				if (
					new_i < 0 or new_j < 0 
					or new_i >= h or new_j >= w
				):
					# visible in this dir
					return True

				if grid[new_i][new_j] >= treeheight:
					# not visible in this dir
					break

		return False

	return sum(1 for i, j in trees if is_visible(i, j))