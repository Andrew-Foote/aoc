from enum import Enum
import functools as ft
import heapq
import itertools as it
import re
from typing import Iterator, Self

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
		cur = graph[cur][direction.index]

def p1(ip: str) -> int:
	dirs, nodes = parse(ip)

	for i, (node, direction) in enumerate(steps(dirs, nodes)):
		if node == STOP:
			return i

def p2_naive(ip: str) -> int:
	dirs, nodes = parse(ip)
	starts = [node for node in nodes.keys() if node.endswith('A')]
	stops = {node for node in nodes.keys() if node.endswith('Z')}
	iterators = [steps(dirs, nodes, start) for start in starts]

	for i in it.count():
		curs = [next(iterator)[0] for iterator in iterators]
		print(curs)

		if all(cur in stops for cur in curs):
			return i

def stop_places(dirs: list[Dir], graph: Graph, start: str, stops: set[str]) -> Iterator[int]:
	print('computing stop_places for start ', start)

	visited = {}
	initial_stop_places = []

	for i, (node, direction) in enumerate(steps(dirs, graph, start)):
		print(node, direction, end = ' . ')

		if (node, direction) in visited:
			repeat_start = visited[node, direction]
			print('revisisted ', node, direction, 'at', i, 'steps, was first visited at', repeat_start)
			repeat_end = i
			break
		else:
			visited[node, direction] = i

			if node in stops:
				print('node in stops: ', node)
				yield i
				initial_stop_places.append(i)
		
	for i, p in enumerate(initial_stop_places):
		if p >= repeat_start:
			stop_places_within_repeat = initial_stop_places[i:]
			break
	else:
		stop_places_within_repeat = []

	if not stop_places_within_repeat:
		return

	for q in it.count(1):
		for p in stop_places_within_repeat:
			yield p + q * (repeat_end - repeat_start)

def p2(ip: str) -> int:
	dirs, nodes = parse(ip)
	starts = [node for node in nodes.keys() if node.endswith('A')]
	stops = {node for node in nodes.keys() if node.endswith('Z')}
	stop_place_iterators = [stop_places(dirs, nodes, start, stops) for start in starts]
	heap = []
	heapq.heapify(heap)

	while True:
		nexts = []

		for iterator in stop_place_iterators:
			try:
				nexts.append(next(iterator))
			except StopIteration:
				nexts.append(float('inf'))

		print('stop_places', nexts)

		for nxt in nexts:
			heapq.heappush(heap, nxt)

		if len(set(heap[:len(starts)])) <= 1:
			return heap[0]
		else:
			fst = heap[0]
			i = 0

			while heap[i] == fst:
				i += 1

			for _ in range(i):
				heapq.heappop(heap)