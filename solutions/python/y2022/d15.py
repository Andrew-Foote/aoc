import re
from typing import Iterator

test_inputs = [('example', '''\
10
20
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3\
''', [
	('p1', '26'),
	('distress_beacon', '(14, 11)'),
	('p2', '56000011')
])]

def parse(ip: str) -> tuple[int, int, list[tuple[complex, complex]]]:
	lines = iter(ip.splitlines())
	y0 = int(next(lines))
	M = int(next(lines))
	data = []

	for line in lines:
		fr = r'(-?\d+)'
		sx, sy, bx, by = map(int, re.match(fr'Sensor at x={fr}, y={fr}: closest beacon is at x={fr}, y={fr}', line).groups())
		data.append((complex(sx, sy), complex(bx, by)))

	return y0, M, data

def manhattan_dist(z1: complex, z2: complex) -> int:
	return int(abs(z2.real - z1.real) + abs(z2.imag - z1.imag))

def p1(ip: str) -> str:
	# manually edit the update to insert 2000000 here
	y0, _, data = parse(ip)
	excluded = set()

	for sensor, beacon in data:
		sx = int(sensor.real)
		sy = int(sensor.imag)
		bx = int(beacon.real)
		by = int(beacon.imag)

		# The excluded points are all those of the form (x, y0), where
		#
		#   d((x, y0), (sx, sy)) <= d((bx, by) - (sx, sy))                     (1)
		#
		# and (x, y0) != (bx, by).
		#
		# Inequality (1) is equivalent to
		#
		#   |x - sx| + |y0 - sy| <= |bx - sx| + |by - sy|
		#
		# To solve for x, we can rearrange this:
		#
		#   |x - sx| <= |bx - sx| + |by - sy| - |y0 - sy|
		#   
		# Let r be the RHS here. This inequality can only hold if the RHS is nonnegative, in which
		# case it is equivalent to
		#
		#   -r <= x - sx <= r
		#
		# i.e.
		#
		#   sx - r <= x <= sx + r.

		r = abs(bx - sx) + abs(by - sy) - abs(y0 - sy)

		if r >= 0:
			s = set(range(sx - r, sx + r + 1))

			if by == y0:
				s.remove(bx)

			excluded.update(s)

	return len(excluded)

def distress_beacon_dumb(ip: str) -> tuple[int, int]:
	_, M, data = parse(ip)

	# The distress beacon is at the unique point (x, y) where 0 <= x <= M, 0 <= y <= m and (x, y)
	# is not detectable by any sensor, i.e. for all sensor-beacon pairs (sx, sy), (bx, by), we have
	#
	#   d((x, y), (sx, sy)) <= d((bx, by) - (sx, sy))
	#
	# and (x, y) != (bx, by).

	for x in range(M + 1):
		for y in range(M + 1):
			#print(x, y)
			for sensor, beacon in data:
				sx = int(sensor.real)
				sy = int(sensor.imag)
				bx = int(beacon.real)
				by = int(beacon.imag)
				d = abs(bx - sx) + abs(by - sy)
				d2 = abs(x - sx) + abs(y - sy)
				#print(f'  {sensor=}, {beacon=}, d(sensor, beacon)={d}, d(sensor, (x, y))={d2}', end = '... ')

				if d2 <= d:
					#print('could be detected')
					break
				else:
					pass
					#print('cannot be detected')
			else:
				return (x, y)

	raise ValueError('could not find')

def dist(z: complex, w: complex) -> int:
	return abs(int(z.real) - int(w.real)) + abs(int(z.imag) - int(w.imag))

def diamond(centre: complex, radius: int) -> Iterator[complex]:
	p = centre - radius * 1j

	for d in ((1 + 1j, -1 + 1j, -1 - 1j, 1 - 1j)):
		assert dist(p, centre) == radius, (p, centre, dist(p, centre), radius)
		yield p

		while True:
			p += d

			if p.real == centre.real or p.imag == centre.imag:
				break

			assert dist(p, centre) == radius, (p, centre, dist(p, centre), radius)
			yield p

def detectable(p: complex, data: list[tuple[complex, complex]]) -> bool:
	for sensor, beacon in data:
		dps = dist(p, sensor)
		dbs = dist(beacon, sensor)

		if dps <= dbs:
			#print(f'  detectable by {sensor=}, {beacon=}, {dps=}, {dbs=}')
			return True
		else:
			pass
			#print(f'  not detectable by {sensor=}, {beacon=}, {dps=}, {dbs=}')

	return False

def distress_beacon(ip: str) -> tuple[int, int]:
	_, M, data = parse(ip)

	# The distress beacon is at the unique point (x, y) where 0 <= x <= M, 0 <= y <= m and (x, y)
	# is not detectable by any sensor, i.e. for all sensor-beacon pairs (sx, sy), (bx, by), we have
	#
	#   d((x, y), (sx, sy)) > d((bx, by) - (sx, sy))
	#
	# Since it is the unique such point, and M > 1, at least one (in fact, two) of the adjacent
	# points will be on the edge of the detection range of a sensor. So it suffices to check just
	# the points 1 out from each of those detection ranges. For each sensor, we can go over each of
	# those points, and check if it is within the detection range of any of the other sensors. If
	# it isn't, we've found the distress beacon.
	#
	# Given a sensor (sx, sy) and beacon (bx, by), the perimeter we need to check is the set of all
	# points (x, y) such that
	#
	# |x - sx| + |y - sy| = r  where r = |bx - sx| + |by - sy| + 1.
	# 
	# Visually this looks like a diamond around (sx, sy) where the corners have an x- or y-
	# coordinate equal to sx or sy and are at a distance of r from (sx, sy). So we can generate
	# those points by starting at, say, (sx, sy - r), repeatedly adding (1, 1) till we get to
	# (sx + r, sy), and so on.

	for sensor, beacon in data:
		r = dist(sensor, beacon) + 1

		for p in diamond(sensor, r):
			if 0 <= p.real <= M and 0 <= p.imag <= M:
				if not detectable(p, data):
					return int(p.real), int(p.imag)

def distress_beacon_hypothetical(ip: str) -> tuple[int, int]:
	# The previous solution is fast enough to give an answer for the real input in ~15 seconds but
	# that's still too slow for my liking. I think we must be able to do better by taking advantage
	# of the fact that each undetectable point has to be adjacent to as many points on the edge of
	# a detectable range as there are adjacent points in the box (the "box" being the one with top
	# left (0, 0) and bottom right (M, M)). So:
	# 
	# - If the point is in the corner of the box, it has to be adjacent to two points on the edge
	#   of a detectable range. But they could be from the same detectable range.
	# - Otherwise, it has to be adjcent to *three* points on the edge of a detectable range. Two of
	#   those will be on opposite sides, and therefore cannot possibly be within the same
	#   detectable range.
	#
	# This suggests what might be a more efficient strategy. First, check whether any of the four
	# corners of the box are undetectable. If not, we know the undetectable point has to be in the
	# intersection of two detectable-range-outer-boundaries. Can we efficiently enumerate the
	# points in such an intersection? Just taking the two diamonds as hash-sets and intersecting
	# them probably wouldn't lead to any gain in efficiency....
	#
	# TODO
	pass

def p2(ip: str) -> int:
	x, y = distress_beacon(ip)
	return x * 4000000 + y