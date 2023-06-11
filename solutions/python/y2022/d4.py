import re
from typing import Iterator
from solutions.python.lib.utils import range_includes, range_intersection

test_inputs = [('example', '''\
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8\
''', [
	('full_include_pairs_csv', '3,4'),
	('p1', '2'),
	('range_intersections_csv', ';;7;3,4,5,6,7;6;4,5,6'),
	('p2', '4')
])]

ElfPair = tuple[range, range]

def pairs(ip: str) -> Iterator[ElfPair]:
	for line in ip.splitlines():
		m = re.match(r'(\d+)-(\d+),(\d+)-(\d+)$', line)
		assert m is not None
		lb1, ub1, lb2, ub2 = m.groups()
		yield range(int(lb1), int(ub1) + 1), range(int(lb2), int(ub2) + 1)

def full_include_pairs(ip: str) -> Iterator[tuple[int, range, range]]:
	for i, (r1, r2) in enumerate(pairs(ip)):
		if range_includes(r1, r2) or range_includes(r2, r1):
			yield i, r1, r2

def full_include_pairs_csv(ip: str) -> str:
	return ','.join(str(i) for i, r1, r2 in full_include_pairs(ip))

def p1(ip: str) -> int:
	return len(list(full_include_pairs(ip)))

def range_intersections(ip: str) -> Iterator[range]:
	for r1, r2 in pairs(ip):
		yield range_intersection(r1, r2)

def range_intersections_csv(ip: str) -> str:
	return ';'.join(','.join(map(str, r)) for r in range_intersections(ip))

def p2(ip: str) -> int:
	return sum(map(bool, range_intersections(ip)))