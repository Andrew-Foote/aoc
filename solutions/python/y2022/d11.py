from abc import ABC, abstractmethod
from collections import deque
from dataclasses import dataclass
import itertools as it
from typing import Iterable, Iterator, Literal

test_inputs = [('example', '''\
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1\
''', [
	('p1', '10605'),
	('p2', '2713310158')
])]

@dataclass
class Monkey:
	items: deque[int]
	op: str
	test: int
	true_branch: int
	false_branch: int
	inspect_count: int=0

	def apply_op(self, worry: int) -> int:
		lopnd, optr, ropnd = self.op.split()
		lopnd = worry if lopnd == 'old' else int(lopnd)
		ropnd = worry if ropnd == 'old' else int(ropnd)
		return {
			'+': lambda x, y: x + y,
			'*': lambda x, y: x * y
		}[optr](lopnd, ropnd)

	def apply_test(self, worry: int) -> bool:
		return worry % self.test == 0

def parse(ip: str) -> Iterator[Monkey]:
	lines = ip.splitlines()

	for i in range(0, len(lines), 7):
		items = lines[i + 1].removeprefix('  Starting items: ')
		items = deque([int(item.strip()) for item in items.split(',')])
		op = lines[i + 2].removeprefix('  Operation: new = ')
		test = int(lines[i + 3].removeprefix('  Test: divisible by '))
		true_branch = int(lines[i + 4].removeprefix('    If true: throw to monkey '))
		false_branch = int(lines[i + 5].removeprefix('    If false: throw to monkey '))
		yield Monkey(items, op, test, true_branch, false_branch)

def play_round(monkeys: list[Monkey]):
	for monkey in monkeys:
		while monkey.items:
			item = monkey.items.popleft()
			monkey.inspect_count += 1
			worry = monkey.apply_op(item) // 3

			if monkey.apply_test(worry):
				monkeys[monkey.true_branch].items.append(worry)
			else:
				monkeys[monkey.false_branch].items.append(worry)

def p1(ip: str) -> int:
	monkeys = list(parse(ip))

	for _ in range(20):
		play_round(monkeys)

	monkeys.sort(key=lambda monkey: monkey.inspect_count)
	return monkeys[-1].inspect_count * monkeys[-2].inspect_count

# @dataclass
# class Worry(ABC):
# 	@abstractmethod
# 	def modulo(self, d: int) -> int:
# 		...

# @dataclass
# class SimpWorry(ABC):
# 	value: int

# 	def modulo(self, d):
# 		return self.value % d

# @dataclass
# class AddWorry(ABC):
# 	base: Worry
# 	addend: int

# 	def modulo(self, d):
# 		# a + b equiv c (mod d)
# 		# iff
# 		# a equiv c - b (mod d)
# 		return (self.base.modulo(d) + self.addend) % d

# @dataclass
# class MulWorry(ABC):
# 	base: Worry
# 	muland: int

# 	def modulo(self, d):
# 		return (self.base.modulo(d) * self.muland) % d

# @dataclass
# class SquareWorry(ABC):
# 	base: Worry

# 	def modulo(self, d):
# 		# given that a equiv c (mod d),
# 		# solve for x in a^2 equiv x (mod d)
# 		# a equiv c (mod d) means a = qd + c
# 		# so a^2 = (qd + c)^2 = q^2 d^2 + 2qdc + c^2
# 		#                     = (q^2 d + 2qc) d + c^2
# 		# so x equiv c^2 (mod d)
# 		return (self.base.modulo(d) ** 2) % d

@dataclass
class Worry:
	rs: dict[int, int]

	def __mod__(self, other: int) -> int:
		return self.rs[other]

	def __add__(self, other: int) -> 'Worry':
		return Worry({
			d: (r + other) % d
			for d, r in self.rs.items() 
		})

	def __mul__(self, other: int) -> 'Worry':
		return Worry({
			d: (r * other) % d
			for d, r in self.rs.items() 
		})		

	def __pow__(self, other: int) -> 'Worry':
		return Worry({
			d: (r ** other) % d
			for d, r in self.rs.items()
		})

@dataclass
class Monkey2:
	items: deque[Worry]
	op: str
	test: int
	true_branch: int
	false_branch: int
	inspect_count: int=0

	def apply_op(self, worry: Worry) -> Worry:
		lopnd, optr, ropnd = self.op.split()

		if lopnd == 'old' and ropnd == 'old' and optr == '*':
			lopnd = worry
			optr = '^'
			ropnd = 2
		else:
			lopnd = worry if lopnd == 'old' else int(lopnd)
			ropnd = worry if ropnd == 'old' else int(ropnd)

		return {
			'+': lambda x, y: x + y,
			'*': lambda x, y: x * y,
			'^': lambda x, y: x ** y
		}[optr](lopnd, ropnd)

	def apply_test(self, worry: Worry) -> bool:
		return worry % self.test == 0

def parse2(ip: str) -> list[Monkey2]:
	lines = ip.splitlines()
	monkeys = []
	starting_items = []

	for i in range(0, len(lines), 7):
		items = lines[i + 1].removeprefix('  Starting items: ')
		starting_items.append([int(item.strip()) for item in items.split(',')])
		op = lines[i + 2].removeprefix('  Operation: new = ')
		test = int(lines[i + 3].removeprefix('  Test: divisible by '))
		true_branch = int(lines[i + 4].removeprefix('    If true: throw to monkey '))
		false_branch = int(lines[i + 5].removeprefix('    If false: throw to monkey '))
		monkeys.append(Monkey2(deque([]), op, test, true_branch, false_branch))

	tests = [monkey.test for monkey in monkeys]

	for monkey, sis in zip(monkeys, starting_items):
		monkey.items.extend(Worry({t: si % t for t in tests}) for si in sis)

	return monkeys

def play_round2(monkeys: list[Monkey2]):
	for monkey in monkeys:
		while monkey.items:
			item = monkey.items.popleft()
			monkey.inspect_count += 1
			worry = monkey.apply_op(item)

			if monkey.apply_test(worry):
				monkeys[monkey.true_branch].items.append(worry)
			else:
				monkeys[monkey.false_branch].items.append(worry)

def p2(ip: str) -> int:
	monkeys = parse2(ip)

	for _ in range(10_000):
		play_round2(monkeys)

	monkeys.sort(key=lambda monkey: monkey.inspect_count)
	return monkeys[-1].inspect_count * monkeys[-2].inspect_count