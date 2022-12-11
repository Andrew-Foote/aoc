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
	('p2', '0')
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
	print(monkeys)
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

def p2(ip: str) -> int:
	return 0