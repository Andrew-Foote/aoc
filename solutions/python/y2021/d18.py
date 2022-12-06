from abc import ABC
from dataclasses import dataclass
import functools as ft
import itertools as it
import math
import operator
import re
from typing import cast, Iterable, Iterator

test_inputs = [
('example', '''\
[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]\
''', []),
	('example2', '[1,1]\n[2,2]\n[3,3]\n[4,4]', [
		('final_sum', '[[[[1,1],[2,2]],[3,3]],[4,4]]')
	]),
	('example3', '[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]', [
		('final_sum', '[[[[3,0],[5,3]],[4,4]],[5,5]]')
	]),
	('example4', '[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]', [
		('final_sum', '[[[[5,0],[7,4]],[5,5]],[6,6]]')
	]),
('example5', '''\
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]\
''', [
	('final_sum', '[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]')
]),
('example6', '''\
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]\
''', [
	('final_sum', '[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]'),
	('p1', '4140')
])
]

SNPath = list[bool]

@dataclass
class SN:
	value: int | tuple['SN', 'SN']

	@property
	def is_reg_num(self) -> bool:
		return isinstance(self.value, int)

	@staticmethod
	def reg_num(value: int) -> 'SN':
		return SN(value)

	def set_reg_num(self, value: int) -> None:
		self.value = value

	@property
	def is_pair(self) -> bool:
		return isinstance(self.value, tuple)

	@property
	def left(self) -> 'SN':
		assert isinstance(self.value, tuple)
		return self.value[0]

	@property
	def right(self) -> 'SN':
		assert isinstance(self.value, tuple)
		return self.value[1]

	@staticmethod
	def pair(left: 'SN', right: 'SN') -> 'SN':
		return SN((left, right))

	def set_pair(self, left: 'SN', right: 'SN') -> None:
		self.value = (left, right)

	def get(self, path: SNPath) -> 'SN':
		node = self

		for dir_ in path:
			node = node.left if dir_ else node.right

		return node

	def __add__(self: 'SN', other: 'SN') -> 'SN':
		res = SN.pair(self, other)
		sn_reduce(res)
		return res

	def __str__(self):
		if self.is_reg_num: return str(self.value)
		return f'[{self.left},{self.right}]'

def parse(ip: str) -> Iterator[SN]:
	for line in ip.splitlines():
		yield sn_parse(line)

def sn_parse(s: str) -> SN:
	int_pat = re.compile(r'\d+')

	def parse_sn(i: int) -> tuple[SN, int]:
		m = int_pat.match(s, i)

		if m is not None:
			return SN.reg_num(int(m.group())), m.end()

		if s[i] != '[':
			raise ValueError(f"'{s}', index {i}: expected a digit or '['")

		left, i = parse_sn(i + 1)
		
		if s[i] != ',':
			raise ValueError(f"'{s}', index {i}: expected ','")

		right, i = parse_sn(i + 1)

		if s[i] != ']':
			raise ValueError(f"'{s}', index {i}: expected ']'")

		return SN.pair(left, right), i + 1

	res, i = parse_sn(0)

	if i != len(s):
		raise ValueError(f"'{s}', index {i}: parsing finished early")

	return res

def sn_walk(sn: SN) -> Iterator[SNPath]:
	yield []

	if sn.is_pair:
		for subpath in sn_walk(sn.left):
			yield [True, *subpath]

		for subpath in sn_walk(sn.right):
			yield [False, *subpath]

def sn_reduce1(root: SN) -> None:
	reduced = False
	last_reg_num_seen = None
	walker = sn_walk(root)
	node_to_split = None

	for path in walker:
		node = root.get(path)

		if node.is_reg_num:
			last_reg_num_seen = node
			value = cast(int, node.value)

			if value >= 10 and node_to_split is None:
				node_to_split = node

		if (
			node.is_pair and len(path) >= 4
			and node.left.is_reg_num and node.right.is_reg_num
		):
			#print(f'exploding at {node} at {path}')

			if last_reg_num_seen is not None:
				assert isinstance(last_reg_num_seen.value, int)
				last_reg_num_seen.value += cast(int, node.left.value)

			right_value = cast(int, node.right.value)
			node.set_reg_num(0)

			for path in walker:
				node = root.get(path)

				if node.is_reg_num:
					#print(f'  right-extended to {node} at {path}')
					assert isinstance(node.value, int)
					node.value += right_value
					break

			reduced = True
			break

	# had to look on reddit to figure out the bug here:
	# https://old.reddit.com/r/adventofcode/comments/rj1oz7/aoc_2021_day_18_part_1_example_input_question/
	# we have to consider explosions before splits
	if not reduced and node_to_split is not None:
		#print(f'splitting at {node_to_split} at {path}')
		value = cast(int, node_to_split.value)
		node_to_split.set_pair(SN.reg_num(math.floor(value / 2)), SN.reg_num(math.ceil(value / 2)))
		reduced = True

	return reduced

def sn_reduce(root: SN) -> None:
	#print(f'reducing {root}')

	while True:
		reduced = sn_reduce1(root)
		#print(f'-> {root}')
		#input()

		if not reduced:
			#print('fin')
			break

def sn_sum(sns: Iterable[SN]) -> SN:
	res = sns[0]

	for sn in sns[1:]:
		res += sn

	return res

def final_sum(ip: str) -> SN:
	s = None

	for sn in parse(ip):
		if s is None:
			s = sn
		else:
			s += sn

	assert s is not None
	return s

def magnitude(sn: SN) -> int:
	if sn.is_reg_num:
		assert isinstance(sn.value, int)
		return sn.value
	else:
		return 3 * magnitude(sn.left) + 2 * magnitude(sn.right)

def p1(ip: str) -> int:
	return magnitude(final_sum(ip))