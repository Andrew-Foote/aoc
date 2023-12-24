from fractions import Fraction as frac
import math
import numpy as np

test_inputs = [
	('example', '''\
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
''', [
		('p1', 2),
		('p2', 1),
	])
]

Vec3d = tuple[int, int, int]
Hailstone = tuple[Vec3d, Vec3d]

def parse(ip: str) -> list[Hailstone]:
	lines = ip.splitlines()
	hailstones = []

	for line in lines:
		if line:
			pos, vel = line.split('@')
			posparts = pos.split(',')
			pos = tuple(int(part.strip()) for part in posparts)
			velparts = vel.split(',')
			vel = tuple(int(part.strip()) for part in velparts)
			hailstones.append((pos, vel))

	return hailstones

def p1(ip: str) -> int:
	hailstones = parse(ip)
	k = 0

	for i, (h1p0, h1v) in enumerate(hailstones):
		for h2p0, h2v in hailstones[i + 1:]:
			print('h1', h1p0, h1v, 'h2', h2p0, h2v)

			h1p0x, h1p0y, _ = h1p0
			h1vx, h1vy, _ = h1v
			h2p0x, h2p0y, _ = h2p0
			h2vx, h2vy, _ = h2v

			# h1p = h1p0 + n * h1v
			# h2p = h2p0 + n * h2v
			# h1p0 + n * h1v = h2p0 + n * h2v
			# n * (h1v - h2v) = h2p0 - h1p0
			# n = (h2p0 - h1p0) / (h1v - h2v)

			# oh wait
			# hailstones themselves don't have to be colliding
			# so n can be different between the two hailstones

			# h1p0 + n1 * h1v = h2p0 + n2 * h2v
			# 
			# h1p0x + n1 * h1vx = h2p0x + n2 * h2vx
			# h1p0y + n1 * h1vy = h2p0y + n2 * h2vy
			# 
			# h1vx * n1 - h2vx * n2 = h2p0x - h1p0x
			# h1vy * n1 - h2vy * n2 = h2p0y - h1p0y
			#
			# (h1vx, -h2vx)(n1) = (h2p0x - h1p0x)
			# (h1vy, -h2vy)(n2)   (h2p0y - h1p0y)

			try:
				s = np.linalg.solve(
					np.array([
						[h1vx, -h2vx],
						[h1vy, -h2vy]
					]),
					np.array([h2p0x - h1p0x, h2p0y - h1p0y])
				)
			except np.linalg.LinAlgError:
				pass
			else:
				n1, n2 = s
				print(n1, n2)
				h1p = h1p0x + n1 * h1vx, h1p0y + n1 * h1vy
				h2p = h2p0x + n2 * h2vx, h2p0y + n2 * h2vy
				print(h1p, h2p)
				#assert h1p == h2p
				p = h1p = h2p

				MIN = 200000000000000
				MAX = 400000000000000

				if n1 >= 0 and n2 >= 0 and MIN <= p[0] <= MAX and MIN <= p[1] <= MAX:
					print('SUCCS')
					k += 1

	return k

def p2(ip: str) -> int:
	return 1