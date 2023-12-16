from enum import Enum
import itertools as it
import re

test_inputs = [('example', '''\
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
''', [
	('p1', 2)
]), ('example2', '''\
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
''', [
	('p1', 6)
])]

START = 'AAA'
STOP = 'ZZZ'

class Dir(Enum):
	LEFT = 'L'
	RIGHT = 'R'

def parse(ip: str) -> tuple[list[Dir], dict[str, tuple[str, str]]]:
	dirs_s, nodes_s = ip.split('\n\n')
	dirs = list(map(Dir, dirs_s.strip()))
	node_ses = nodes_s.strip().splitlines()
	nodes = {}

	for node_s in node_ses:
		m = re.match(r'\s*(\w+)\s*=\s*\(\s*(\w+),\s*(\w+)\s*\)\s*', node_s)
		if m is None: breakpoint()
		label, left, right = m.groups()
		nodes[label] = (left, right)

	return dirs, nodes

def p1(ip: str) -> int:
	dirs, nodes = parse(ip)
	node = START
	steps = 0

	for direction in it.cycle(dirs):
		if node == STOP:
			return steps

		match direction:
			case Dir.LEFT:
				node = nodes[node][0]
			case Dir.RIGHT:
				node = nodes[node][1]

		steps += 1