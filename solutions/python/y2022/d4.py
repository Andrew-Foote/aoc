import re
from typing import Iterator
from utils import joinlines

test_inputs = [('example', '''\
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8\
''', [
	('full_contain_pairs_csv', '3,4'),
	('p1', '2') 
])]

ElfPair = tuple[range, range]

def pairs(ip: str) -> Iterator[ElfPair]:
	for lb1, ub1, lb2, ub2 in [
		re.match(r'^(\d+)-(\d+),(\d+)-(\d+)$', line).groups()
		for line in ip.splitlines()
	]:
		yield range(int(lb1), int(ub1) + 1), range(int(lb2), int(ub2) + 1)

def range_contains(bigrange: range, lilrange: range) -> bool:
	return (
		bigrange.start <= lilrange.start
		and
		bigrange.stop >= lilrange.stop
	)

def full_contain_pairs(ip: str) -> Iterator[tuple[int, range, range]]:
	for i, (r1, r2) in enumerate(pairs(ip)):
		if range_contains(r1, r2) or range_contains(r2, r1):
			yield i, r1, r2

def full_contain_pairs_csv(ip: str) -> str:
	return ','.join(str(i) for i, r1, r2 in full_contain_pairs(ip))

def p1(ip: str) -> int:
	return sum(1 for _ in full_contain_pairs(ip))
