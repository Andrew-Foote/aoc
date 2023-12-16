from enum import Enum
import itertools as it
import re
from typing import Self

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
]), ('example3', '''\
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
''', [
	('p2', 6)
])]

START = 'AAA'
STOP = 'ZZZ'

class Dir(Enum):
	LEFT = 'L'
	RIGHT = 'R'

	@property
	def index(self: Self) -> int:
		match self:
			case Dir.LEFT:
				return 0
			case Dir.RIGHT:
				return 1

Graph = dict[str, tuple[str, str]]

def parse(ip: str) -> tuple[list[Dir], Graph]:
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

def steps(dirs: list[Dir], graph: Graph, start: str=START) -> Iterator[tuple[str, Dir]]:
	cur = start

	for direction in it.cycle(dirs):
		yield cur, direction
		cur = graph[cur][direcion.index]

def p1(ip: str) -> int:
	dirs, nodes = parse(ip)

	for i, (node, direction) in enumerate(steps(dirs, nodes)):
		if node == STOP:
			return i

def p2_naive(ip: str) -> int:
	dirs, nodes = parse(ip)
	curs = [node for node in nodes.keys() if node.endswith('A')]
	stops = {node for node in nodes.keys() if node.endswith('Z')}
	steps = 0

	for direction in it.cycle(dirs):
		print(curs)

		if all(cur in stops for cur in curs):
			return steps

		for i, cur in enumerate(curs):
			curs[i] = nodes[cur][direction.index]

		steps += 1

def stop_places(dirs: list[Dir], graph: Graph, start: str) -> Iterator[int]:
	stops = {node for node in nodes.keys() if node.endswith('Z')}
	cur = start
	visited = {}
	steps = 0
	initial_stop_places = set()

	for direction in it.cycle(dirs):
		visited[cur] = steps

		if cur in stops:
			yield steps
			initial_stop_places.add(steps)

		nxt = nodes[cur][direction.index]

		if nxt in visited:
			repeating_bit_start = visited[nxt]
			repeating_bit_end = steps
			break
		elif 

def p2(ip: str) -> int:
	dirs, nodes = parse(ip)
	starts = [node for node in nodes.keys() if node.endswith('A')]
	stops = {node for node in nodes.keys() if node.endswith('Z')}
	z_indices = []
	repeating_bits = []

	for start in starts:
		cur = start
		visited = {}
		steps = 0
		z_indices_for_this_start = set()
		repeating_bit = None

		for direction in it.cycle(dirs):
			visited[cur] = steps
			nxt = nodes[cur][direction.index]

			if nxt in visited:
				repeating_bit = (visited[nxt], steps)
				break
			elif nxt in stops:
				z_indices_for_this_start.add(steps)
			else:
				cur = nxt

			steps += 1

		z_indices.append(z_indices_for_this_start)
		repeating_bits.append(repeating_bit)

	# so for each starting node, we have
	# n = index at which the repeating part of the sequence starts
	# l = length of repeating part
	# zs = indices at which stop is possible (all <= n + l)

	# stops are possible at:
	# - any indices equal to a z-value less than n
	# - any indices i such that i - n modulo l is equal to a z-value

	def all_z_indices(i):



	# for each node we follow the path till we come back to the same node with the same starting direction
	# we note at what indices we are at stops


	for direction in it.cycle(dirs):
		for i, cur in enumerate(curs):
			if sequences[i][-1] == sequences[i][0]: # sequence is "complete"
				cur = 

			new_cur = nodes[cur][direction.index]