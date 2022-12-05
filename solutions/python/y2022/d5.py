import re
from typing import Iterator
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
	('stacks_csv', '1,Z,N;2,M,C,D;3,P'),
	('proc_csv', '1,2,1;3,1,3;2,2,1;1,1,2'),
	('stacks_phases_csv', joinlines([
		'1,Z,N,D;2,M,C;3,P',
		'1;2,M,C;3,P,D,N,Z',
		'1,C,M;2;3,P,D,N,Z',
		'1,C;2,M;3,P,D,N,Z'
	])),
	('p1', 'CMZ')
])]

Stacks = dict[int, list[int]]
Proc = list[tuple[int, int, int]]

def parse(ip: str) -> tuple[Stacks, Proc]:
	lines = ip.splitlines()
	breakp = lines.index('')
	i = breakp - 1
	stacks = {}

	for d in map(int, re.findall(r'\d+', lines[i])):
		stacks[d] = []

	while True:
		i -= 1
		if i < 0: break
		line = lines[i]

		for j in range(0, len(line), 4):
			if line[j] == '[':
				stacks[j // 4 + 1].append(line[j + 1])

	proc = []

	for line in lines[breakp + 1:]:
		count, src, dst = map(int, re.match(r'^move (\d+) from (\d+) to (\d+)$', line).groups())
		proc.append((count, src, dst))

	return stacks, proc

def stacks_as_csv(stacks: Stacks) -> str:
	return ';'.join(','.join((str(i), *stacks[i])) for i in sorted(stacks.keys()))

def stacks_csv(ip: str) -> str:
	stacks, proc = parse(ip)
	return stacks_as_csv(stacks)

def proc_csv(ip: str) -> str:
	stacks, proc = parse(ip)
	return ';'.join(','.join(map(str, instr)) for instr in proc)

def apply_instr(stacks: Stacks, instr: tuple[int, int, int]) -> None:
	count, src, dst = instr

	for _ in range(count):
		stacks[dst].append(stacks[src].pop())

def stacks_phases(stacks: Stacks, proc: Proc) -> Iterator[Stacks]:
	for instr in proc:
		apply_instr(stacks, instr)
		current_stacks = {i: stack.copy() for i, stack in stacks.items()}
		yield current_stacks

def stacks_phases_csv(ip: str) -> str:
	stacks, proc = parse(ip)
	return joinlines(stacks_as_csv(curstacks) for curstacks in stacks_phases(stacks, proc))

def p1(ip: str) -> str:
	stacks, proc = parse(ip)
	list(stacks_phases(stacks, proc))
	return ''.join(stacks[i][-1] for i in sorted(stacks.keys()))
