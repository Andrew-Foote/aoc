from abc import ABC
from dataclasses import dataclass
import functools as ft
import itertools as it
import math
import operator
from typing import Iterator

test_inputs = [('example', '''\
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
	value: int | list['SN', 'SN']

	@property
	def is_reg_num(self) -> bool:
		return isinstance(self.value, int)

	@staticmethod
	def reg_num(value: int) -> 'SN':
		return SN(value)

	@property
	def is_pair(self) -> bool:
		return isinstance(self.value, list)

	@property
	def left(self) -> 'SN':
		return self.value[0]

	@property
	def right(self) -> 'SN':
		return self.value[1]

	@staticmethod
	def pair(left: 'SN', right: 'SN') -> 'SN':
		return SN([left, right])

	def get(self, path: SNPath) -> None:
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
	# need to fix this parser
	s = s.lstrip('[')
	s = s.rstrip(']')
	parts = s.split(',')
	if len(parts) == 1: return SN.reg_num(int(parts[0]))
	if len(parts) > 2:
		print()
		print('ALERRT!!!')
		print()
		print(parts)
		print()
		print('ALERTT!!!!')
		print()
	left, right = map(sn_parse, parts)
	return SN.pair(left, right)

def sn_walk(sn: SN) -> Iterator[SNPath]:
	yield []

	if sn.is_pair:
		for subpath in sn_walk(sn.left):
			yield [True, *subpath]

		for subpath in sn_walk(sn.right):
			yield [False, *subpath]

def sn_reduce(root: SN) -> SN:
	while True:
		reduced = False
		last_reg_num_seen = None
		walker = sn_walk(root)

		for path in walker:
			node = root.get(path)

			if node.is_reg_num:
				last_reg_num_seen = node

				if node.value > 10:
					node.value = [math.floor(node.value / 2), math.ceil(node.value / 2)]
					reduced = True
					break

			elif (
				node.is_pair and len(path) >= 4
				and node.left.is_reg_num and node.right.is_reg_num
			):
				if last_reg_num_seen is not None: last_reg_num_seen.value += node.left.value	
				right_value = node.right.value
				node.value = 0
				left_subpath, right_subpath = it.islice(walker, 2)

				for path in walker:
					node = root.get(path)

					if node.is_reg_num:
						node.value += right_value
						break

				reduced = True
				break

		if not reduced: break

def final_sum(ip: str) -> SN:
	s = None

	for sn in parse(ip):
		if s is None:
			s = sn
		else:
			s += sn

	return s

def magnitude(sn: SN) -> int:
	if sn.is_reg_num:
		return sn.value
	else:
		return 3 * magnitude(sn.left) + 2 * magnitude(sn.right)

def p1(ip: str) -> int:
	return magnitude(final_sum(ip))