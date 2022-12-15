import re
from typing import Iterator

test_inputs = [('example', '''\
10
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
	('p2', '0')
])]

def parse(ip: str) -> tuple[int, list[tuple[complex, complex]]]:
	lines = iter(ip.splitlines())
	y0 = int(next(lines))
	data = []

	for line in lines:
		fr = r'(-?\d+)'
		sx, sy, bx, by = map(int, re.match(fr'Sensor at x={fr}, y={fr}: closest beacon is at x={fr}, y={fr}', line).groups())
		data.append((complex(sx, sy), complex(bx, by)))

	return y0, data

def manhattan_dist(z1: complex, z2: complex) -> int:
	return int(abs(z2.real - z1.real) + abs(z2.imag - z1.imag))

def p1(ip: str) -> str:
	# manually edit the update to insert 2000000 here
	y0, data = parse(ip)
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
		s = set(range(sx - r, sx + r + 1))

		if by == y0:
			s.remove(bx)

		excluded.update(s)

	return len(excluded)

def p2(ip: str) -> int:
	return 0