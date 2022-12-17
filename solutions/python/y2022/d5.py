from dataclasses import dataclass
import re
from typing import Callable, Iterator, Self
from utils import joinlines

test_inputs = [('example', '''\
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
''', [
	('initial_stacks_csv', '1,Z,N;2,M,C,D;3,P'),
	('proc_csv', '1,2,1;3,1,3;2,2,1;1,1,2'),
	('stacks_phases_csv', joinlines([
		'1,Z,N,D;2,M,C;3,P',
		'1;2,M,C;3,P,D,N,Z',
		'1,C,M;2;3,P,D,N,Z',
		'1,C;2,M;3,P,D,N,Z'
	])),
	('p1', 'CMZ'),
	('stacks_phases_p2_csv', joinlines([
		'1,Z,N,D;2,M,C;3,P',
		'1;2,M,C;3,P,Z,N,D',
		'1,M,C;2;3,P,Z,N,D',
		'1,M;2,C;3,P,Z,N,D'
	])),
])]

Stacks = dict[int, list[str]]

@dataclass
class Instruction:
	count: int
	src: int
	dst: int

	def csv(self: Self) -> str:
		return f'{self.count},{self.src},{self.dst}'

	def p1_apply(self: Self, stacks: Stacks) -> None:
		for _ in range(self.count):
			stacks[self.dst].append(stacks[self.src].pop())

	def p2_apply(self: Self, stacks: Stacks) -> None:
		stacks[self.dst].extend(stacks[self.src][-self.count:])
		del stacks[self.src][-self.count:]

Proc = list[Instruction]

def parse(ip: str) -> tuple[Stacks, Proc]:
	lines = ip.splitlines()
	break_i = lines.index('')
	i = break_i - 1
	stacks: Stacks = {int(d): [] for d in re.findall(r'\d+', lines[i])}

	for line in lines[i - 1::-1]:
		for j in range(0, len(line), 4):
			if line[j] == '[':
				stacks[j // 4 + 1].append(line[j + 1])

	proc = []

	for line in lines[break_i + 1:]:
		m = re.match(r'^move (\d+) from (\d+) to (\d+)$', line)
		assert m is not None
		count, src, dst = map(int, m.groups())
		proc.append(Instruction(count, src, dst))

	return stacks, proc

def stacks_csv(stacks: Stacks) -> str:
	return ';'.join(','.join(map(str, (i, *stacks[i]))) for i in sorted(stacks.keys()))

def initial_stacks_csv(ip: str) -> str:
	stacks, proc = parse(ip)
	return stacks_csv(stacks)

def proc_csv(ip: str) -> str:
	stacks, proc = parse(ip)
	return ';'.join(instr.csv() for instr in proc)

def stacks_phases(
	stacks: Stacks, proc: Proc, applier: Callable[[Instruction, Stacks], None]
) -> Iterator[Stacks]:

	for instr in proc:
		applier(instr, stacks)
		current_stacks = {i: stack.copy() for i, stack in stacks.items()}
		yield current_stacks

def stacks_phases_csv(ip: str) -> str:
	stacks, proc = parse(ip)
	
	return joinlines(
		stacks_csv(curstacks)
		for curstacks in stacks_phases(stacks, proc, Instruction.p1_apply)
	)

def p1(ip: str) -> str:
	stacks, proc = parse(ip)
	list(stacks_phases(stacks, proc, Instruction.p1_apply))
	return ''.join(stacks[i][-1] for i in sorted(stacks.keys()))

def stacks_phases_p2_csv(ip: str) -> str:
	stacks, proc = parse(ip)
	
	return joinlines(
		stacks_csv(curstacks)
		for curstacks in stacks_phases(stacks, proc, Instruction.p2_apply)
	)

def p2(ip: str) -> str:
	stacks, proc = parse(ip)
	list(stacks_phases(stacks, proc, Instruction.p2_apply))
	return ''.join(stacks[i][-1] for i in sorted(stacks.keys()))
