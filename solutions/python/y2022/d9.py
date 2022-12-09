from typing import Iterable, Iterator

test_inputs = [('example', '''\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2\
''', [
	('p1', '13'),
	('p2', '1')
]), ('example2', '''\
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20\
''', [
	('p2', '36')
])]

def parse(ip: str) -> Iterator[tuple[str, int]]:
	for line in ip.splitlines():
		ds, count = line.split(' ')
		yield ds, int(count)

DIRMAP = {
	'R': (0, 1),
	'U': (-1, 0),
	'L': (0, -1),
	'D': (1, 0)
}

Point = tuple[int, int]

def step(moves: Iterable[tuple[str, int]]) -> Iterator[tuple[Point, Point]]:
	hpos = (0, 0)
	tpos = (0, 0)

	#print(hpos, tpos)
	yield hpos, tpos

	for ds, count in moves:
		#print('move:', ds, count)
		d1 = DIRMAP[ds]

		for _  in range(count):
			hpos = (hpos[0] + d1[0], hpos[1] + d1[1])
			ydif = hpos[0] - tpos[0]
			xdif = hpos[1] - tpos[1]
			yunit = ydif // abs(ydif) if ydif else 0
			xunit = xdif // abs(xdif) if xdif else 0

			if abs(ydif) > 1 or abs(xdif) > 1:
				tpos = (tpos[0] + yunit, tpos[1] + xunit)

			yield hpos, tpos
			#print(diagram(hpos, tpos))
			#print()

		# d = (d1[0] * count, d1[1] * count)
		# print(d, end = ' : ')
		# hpos = (hpos[0] + d[0], hpos[1] + d[1])

		# ydif = hpos[0] - tpos[0]
		# xdif = hpos[1] - tpos[1]
		# print(ydif, xdif, end = ' : ')
		# yunit = ydif // abs(ydif) if ydif else 0
		# xunit = xdif // abs(xdif) if xdif else 0

		# if abs(ydif) > 1 or abs(xdif) > 1:
		# 	tpos = (tpos[0] + yunit * (abs(ydif) - 1), tpos[1] + xunit * (abs(xdif) - 1))

		# print(hpos, tpos)
		# print(diagram(hpos, tpos))
		# yield hpos, tpos

def diagram(hpos: Point, tpos: Point) -> str:
	ymin = min(hpos[0], tpos[0], 0)
	ymax = max(hpos[0], tpos[0], 0)
	xmin = min(hpos[1], tpos[1], 0)
	xmax = max(hpos[1], tpos[1], 0)

	lines = []

	for i in range(ymin, ymax + 1):
		chars = []

		for j in range(xmin, xmax + 1):
			if (i, j) == hpos:
				chars.append('H')
			elif (i, j) == tpos:
				chars.append('T')
			else:
				chars.append('.')

		lines.append(''.join(chars))

	return '\n'.join(lines)


# 0 |B|
# 1 |TH|
# 2 |T H | -> | TH|
# 3 | T H| -> |  TH|
# 4 |  T H| -> |   TH|


def p1(ip: str) -> int:
	# num positions tail visited at least once
	visited = set()

	for hpos, tpos in step(parse(ip)):
		visited.add(tpos)

	return len(visited)

def step2(moves: Iterable[tuple[str, int]]) -> Iterator[list[Point]]:
	knotpos = [(0, 0) for _ in range(10)]
	# head is knotpos[0], tail is knotpos[9]

	#print(hpos, tpos)
	#yield knotpos

	for ds, count in moves:
		#print('move:', ds, count)

		for _  in range(count):
			#print('nexstep')
			d1 = DIRMAP[ds]
	
			for i, pos in enumerate(knotpos):
				#print('knot number', i, 'currently at', pos)
				knotpos[i] = (pos[0] + d1[0], pos[1] + d1[1])
				#print('now at', knotpos[i])

				if i < len(knotpos) - 1:
					#print('should next knot move?')
					ydif = knotpos[i][0] - knotpos[i + 1][0]
					xdif = knotpos[i][1] - knotpos[i + 1][1]
					yunit = ydif // abs(ydif) if ydif else 0
					xunit = xdif // abs(xdif) if xdif else 0
					#print('ydif', ydif, 'xdif', xdif, 'yunit', yunit, 'xunit', xunit)

					if abs(ydif) > 1 or abs(xdif) > 1:
						d1 = (yunit, xunit)
						#print('yes should move by', d1)
					else:
						d1 = (0, 0)

			#print(diagram2(knotpos))
			#input()
			yield knotpos

def diagram2(knotpos: list[Point]) -> str:
	ymin = min(*(pos[0] for pos in knotpos), 0)
	ymax = max(*(pos[0] for pos in knotpos), 0)
	xmin = min(*(pos[1] for pos in knotpos), 0)
	xmax = max(*(pos[1] for pos in knotpos), 0)

	lines = []

	for i in range(ymin, ymax + 1):
		chars = []

		for j in range(xmin, xmax + 1):
			for k, pos in enumerate(knotpos):
				print(pos)
				if (i, j) == pos:
					sk = str(k)
					if sk == '0': sk = 'H'
					if sk == '9': sk = 'T'
					chars.append(sk)
					break
			else:
				chars.append('.')

		lines.append(''.join(chars))

	return '\n'.join(lines)

def p2(ip: str) -> int:
	# num positions tail visited at least once
	visited = set()

	for knotpos in step2(parse(ip)):
		visited.add(knotpos[9])

	return len(visited)