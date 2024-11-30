from dataclasses import dataclass
from enum import Enum
import itertools as it
import re
from typing import assert_never, Iterator
from solutions.python.lib import prop

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

InstructSet = list[Instruct]

def parse_instructs(s: str) -> Iterator[Instruct]:
	while True:
		for c in s:
			yield Instruct(c)

@dataclass(frozen=True)
class Node:
	label: str

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
	instructs: Iterator[Instruct], network: Network,
	start: Node=START_NODE
) -> Iterator[Node]:

	cur = start

	for instruct in instructs:
		elems = network[cur]
	
		if cur == END_NODE:
			return
		else:
			yield cur

		cur = elems[instruct.index()]

def p1(ip: str) -> int:
	instructs, network = parse(ip)
	return sum(1 for _ in follow_instructs(instructs, network))	

def path(
	instructs: InstructSet, network: Network, start: Node
) -> tuple[list[Node], Node]:

	# The next node is determined by the current node and the current
	# instruction.
	#
	# So we can think of it is a path consisting of (node, instruction) pairs.
	# 
	# Once we reach a (node, instruction) pair that's already been seen, the
	# sequence will repeat in a cycle.

	path: list[Node] = []
	seen: set[tuple[Instruct, Node]] = set()

	nodes = follow_instructs(instructs, network, start)

	for instruct, node in zip(instructs, nodes):
		if (instruct, node) in seen:
			return path, node
		else:
			path.append(node)

@dataclass(frozen=True)
class Cong:
	rhs: int
	modulus: int

def p2(ip: str) -> int:
	instructs, network = parse(ip)
	starts = [node for node in network.keys() if node.endswith('A')]
	ends = [node for node in network.keys() if node.endswith('Z')]
	paths = [path(instructs, network, start) for start in starts]

	# For each start node, we can determine a path through the
	# (node, instruction) pairs that ends in a cycle.
	# 
	# We want to find the number of steps we have to take before
	# all paths are at an end node.
	#
	# Each path can be divided into a pre-cycle and a cycle.
	#
	# First, we should take the max length of any pre-cycle, and
	# iterate up to that number, manually checking for stopping
	# points, just in case a stopping point occurs within one of
	# the pre-cycles for some node.
	#
	# After that we're looking for a stopping point within a cycle.
	# Given a cycle, we can find the set of indexes in that cycle
	# where it's at a node ending in 'Z'.
	#
	# Let S be that set, let m be the length of the pre-cycle, let
	# n be the length of the cycle. The overall index i places the
	# path for this start node at a stopping point iff
	#
	#   (i - m) % n in S
	#
	# i.e.
	# 
	#   i - m = a1 (mod n)  or i - m = a2 (mod n)
	#
	# i.e.
	#  
	#   i = a1 + m (mod n)  or  i = a2 + m (mod n)
	#
	# So we have a conjunction of disjunctions of monic linear congruences.
	# We can distribute the conjunctions through to get a bunch of systems
	# of linear congruences which we can solve.
	# i.e.
	#
	#   i == m

	index = Var()
	formula = TrueProp()

	for path in paths:
		modulus = path.cycle_len
		djclause = FalseProp()

		for stop_index in path.stop_indices_within_cycle:
			djclause |= Lit(Cong(index, modulus + stop_index))

		formula &= djclause

	cjclauses = formula.dnf()

	for cjclause in cjclauses:
		sol = solve_lincongs(cjclause)

		if sol is not None:
			return sol

def solve_lincongs(eqns: list[Cong]) -> int:
	for rhs, modulus in eqns:
