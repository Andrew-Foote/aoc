from dataclasses import dataclass
from enum import Enum
import itertools as it
import re
from typing import assert_never, Iterator
from solutions.python.lib import numth, prop

test_inputs = [
('example', '''\
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
''', [
	('first_10_instructs_csv', 'R,L,R,L,R,L,R,L,R,L'),
	('p1', 2),
	('paths_csv', 'AAA,[],2,[0,1],2'),
]), ('example2', '''\
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
''', [
	('p1', 6),
	('paths_csv', 'AAA,[],6,[0,1,2],3'),
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
	('paths_csv', '11A,[],1,[1],2;22A,[],1,[2,5],6'),
	('p2', 6),
])]

class Instruct(Enum):
	LEFT = 'L'
	RIGHT = 'R'

	def __str__(self) -> str:
		return self.value

	def index(self) -> int:
		match self:
			case Instruct.LEFT:
				return 0
			case Instruct.RIGHT:
				return 1
			case _:
				assert_never(self)

@dataclass(frozen=True)
class InstructSet:
	instructs: list[Instruct]

	def __iter__(self) -> Iterator[Instruct]:
		while True:
			for instruct in self.instructs:
				yield instruct#

	def __len__(self) -> int:
		return len(self.instructs)

	def __getitem__(self, index: int) -> Instruct:
		return self.instructs[index % len(self)]

def parse_instructs(s: str) -> InstructSet:
	return InstructSet(list(map(Instruct, s)))

@dataclass(frozen=True)
class Node:
	label: str

	def is_start(self) -> bool:
		return self.label.endswith('A')

	def is_end(self) -> bool:
		return self.label.endswith('Z')

START_NODE = Node('AAA')
END_NODE = Node('ZZZ')

Network = dict[Node, tuple[Node, Node]]

def parse_network(s: str) -> Network:
	lines = s.splitlines()
	result: Network = {}

	for line in lines:
		m = re.match(r'\s*(\w+)\s*=\s*\(\s*(\w+),\s*(\w+)\s*\)\s*', line)

		if m is None:
			raise ValueError(f'unparseable line in network: {line}')

		node, left, right = m.groups()
		result[Node(node)] = (Node(left), Node(right))

	return result

def parse(ip: str) -> tuple[InstructSet, Network]:
	instructs, network = ip.split('\n\n')
	return parse_instructs(instructs), parse_network(network)

def first_10_instructs_csv(ip: str) -> str:
	instructs, network = ip.split('\n\n')
	return ','.join(map(str, it.islice(parse_instructs(instructs), 10)))

def follow_instructs(
	instructs: InstructSet, network: Network, start: Node=START_NODE
) -> Iterator[Node]:

	cur = start

	for instruct in instructs:
		elems = network[cur]
		yield cur
		cur = elems[instruct.index()]

def p1(ip: str) -> int:
	instructs, network = parse(ip)
	
	return sum(1 for _ in it.takewhile(
		lambda node: node != END_NODE,
		follow_instructs(instructs, network)
	))
	
@dataclass
class Path:
	pre_cycle_stops: set[int]
	pre_cycle_len: int
	cycle_stops: set[int]
	cycle_len: int

def get_path(
	instructs: InstructSet, network: Network, start: Node
) -> Path:

	# The path of the camel can be determined if you know both the node it's
	# currently on, and the index (within the instruction set) of the
	# instruction it's going to follow. We need the index, not just the
	# instruction itself, because the same instruction may appear multiple
	# times within the instruction set with different subsequent instructions.
	# Thus we can represent the camel's "state" as an (instruct_index, node)
	# tuple. Since there are only finitely many such tuples, the path will
	# inevitably end in a cycle.

	seen: dict[tuple[int, Node], int] = {}

	stops: set[int] = set()

	nodes = follow_instructs(instructs, network, start)
	instruct_count = len(instructs)

	for i, node in enumerate(nodes):
		if node.is_end():
			stops.add(i)

		instruct_index = i % instruct_count

		if (instruct_index, node) in seen:
			pre_cycle_len = seen[instruct_index, node]

			pre_cycle_stops = set()
			cycle_stops = set()

			for stop in stops:
				if stop < pre_cycle_len:
					pre_cycle_stops.add(stop)
				elif stop < i:
					cycle_stops.add(stop - pre_cycle_len)

			cycle_len = i - pre_cycle_len
			return Path(pre_cycle_stops, pre_cycle_len, cycle_stops, cycle_len)
		else:
			seen[instruct_index, node] = i

	assert False

def paths_csv(ip: str) -> str:
	instructs, network = parse(ip)
	starts = [node for node in network.keys() if node.is_start()]
	paths = {start: get_path(instructs, network, start) for start in starts}
	res = []

	for start, path in paths.items():
		res.append(','.join([
			start.label,
			'[{}]'.format(','.join(map(str, sorted(path.pre_cycle_stops)))),
			str(path.pre_cycle_len),
			'[{}]'.format(','.join(map(str, sorted(path.cycle_stops)))),
			str(path.cycle_len),
		]))

	return ';'.join(res)

def p2(ip: str) -> int:
	instructs, network = parse(ip)
	starts = [node for node in network.keys() if node.is_start()]
	paths = [get_path(instructs, network, start) for start in starts]

	# For each start node, we can determine a path through the
	# (instruct_index, node) pairs that ends in a cycle.
	# 
	# We want to find the number of steps we have to take before all paths are
	# at an end node.
	#
	# Each path can be divided into a pre-cycle and a cycle.
	#
	# First, we should take the max length of any pre-cycle, and iterate up to
	# that number, manually checking for stopping points, just in case a
	# stopping point occurs within one of the pre-cycles for some node.

	max_pre_cycle_len = max(path.pre_cycle_len for path in paths)
	iterators = [follow_instructs(instructs, network, start) for start in starts]

	for i, *nodes in zip(range(max_pre_cycle_len), *iterators):
		if all(node.is_end() for node in nodes):
			return i

	# After that we're looking for a stopping point within a cycle. Given a
	# cycle, we can find the set of indexes within that cycle (relative to its
	# start point) that are at a stopping point.
	#
	# Let S be that set, let m be the length of the pre-cycle, let n be the
	# length of the cycle, and let i be the current index we're at in the path.
	# We're at a stopping point iff
	#
	#   i - m) % n in S  i.e.  i == a % n (for each a in S)
	#
	# So we have a conjunction of disjunctions of monic linear congruences. We
	# can distribute the conjunctions through to get a bunch of systems of
	# linear congruences, which we can solve.

	formula: prop.Prop[numth.Cong] = prop.TrueProp()

	for path in paths:
		offset = path.pre_cycle_len
		modulus = path.cycle_len
		djclause: prop.Prop[numth.Cong] = prop.FalseProp()

		for stop_index in path.cycle_stops:
			djclause |= prop.Lit(numth.Cong(offset + stop_index, modulus))

		formula &= djclause

	cjclauses = prop.dnf(formula)
	sols: list[int] = []

	for cjclause in cjclauses:
		congs = [lit.val for lit in cjclause]
		sol = numth.solve_congs(congs)

		if sol is not None:
			sols.append(sol.minsol(lb=max_pre_cycle_len))

	return min(sols)