from collections import Counter
from enum import Enum
import functools as ft
import math
import heapq
import itertools as it
import re
from typing import Iterator, Self
from solutions.python.lib.utils import prod

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

def steps(dirs: list[Dir], graph: Graph, start: str=START) -> Iterator[tuple[str, int]]:
	cur = start

	while True:
		for i, direction in enumerate(dirs):
			yield cur, i
			cur = graph[cur][direction.index]

def p1(ip: str) -> int:
	dirs, nodes = parse(ip)

	for i, (node, dirindex) in enumerate(steps(dirs, nodes)):
		if node == STOP:
			return i

def p2_naive(ip: str) -> int:
	dirs, nodes = parse(ip)
	starts = [node for node in nodes.keys() if node.endswith('A')]
	stops = {node for node in nodes.keys() if node.endswith('Z')}
	iterators = [steps(dirs, nodes, start) for start in starts]

	for i in it.count():
		curs = [next(iterator)[0] for iterator in iterators]
		#print(curs)

		if all(cur in stops for cur in curs):
			return i

def cycle_info(dirs: list[Dir], graph: Graph, start: str, stops: set[str]) -> tuple[int, int, list[int]]:
	#print('computing stop_places for start ', start)

	visited = {}
	initial_stop_places = []

	for i, (node, dirindex) in enumerate(steps(dirs, graph, start)):
		#print(node, dirindex, end = ' . ')

		if (node, dirindex) in visited:
			repeat_start = visited[node, dirindex]
			#print('revisisted ', node, dirindex, 'at', i, 'steps, was first visited at', repeat_start)
			repeat_end = i
			break
		else:
			visited[node, dirindex] = i

			if node in stops:
				print('found stop place: ', i, node, dirindex)
				initial_stop_places.append(i)
		
	for i, p in enumerate(initial_stop_places):
		if p >= repeat_start:
			stop_places_within_repeat = initial_stop_places[i:]
			break
	else:
		stop_places_within_repeat = []

	print('cycle info:', start, repeat_start, repeat_end, stop_places_within_repeat)
	return repeat_start, repeat_end, initial_stop_places, stop_places_within_repeat

def stop_places(dirs: list[Dir], graph: Graph, start: str, stops: set[str]) -> Iterator[int]:
	repeat_start, repeat_end, initial_stop_places, stop_places_within_repeat = cycle_info(dirs, graph, start, stops)
	yield from initial_stop_places

	if not stop_places_within_repeat:
		return

	for q in it.count(1):
		for p in stop_places_within_repeat:
			yield p + q * (repeat_end - repeat_start)

# this would be the "real" solution for all inputs
# however it's too inefficient
def p2_try2(ip: str) -> int:
	dirs, nodes = parse(ip)
	starts = [node for node in nodes.keys() if node.endswith('A')]
	stops = {node for node in nodes.keys() if node.endswith('Z')}
	stop_place_iterators = [stop_places(dirs, nodes, start, stops) for start in starts]

	for i, iterator in enumerate(stop_place_iterators):
		print(starts[i], list(it.islice(iterator, 100)))

	stop_place_iterators = [stop_places(dirs, nodes, start, stops) for start in starts]

	counts = Counter()

	while True:
		# print(counts)
		# input()

		curs = [next(iterator) for iterator in stop_place_iterators]

		for cur in curs:
			counts[cur] += 1

			if counts[cur] == len(starts):
				return cur

# turns out, for the input, there is always exactly one stop place within the repeat,
# and its index is equal to the repeat_end minus the repeat_start
# meaning the stop places just occur at multiples of (repeat_end - repeat_start)
# meaning to find the stop place for all starts, we can just take the LCM

def p2(ip: str) -> int:
	dirs, nodes = parse(ip)
	starts = [node for node in nodes.keys() if node.endswith('A')]
	stops = {node for node in nodes.keys() if node.endswith('Z')}
	cycle_infos = [cycle_info(dirs, nodes, start, stops) for start in starts]
	ms = []

	for ci in cycle_infos:
		repeat_start, repeat_end, initial_stop_places, stop_places_within_repeat = ci
		m = repeat_end - repeat_start
		ms.append(m)

	return math.lcm(*ms)
	# frst attempt: 84314908087948949100671