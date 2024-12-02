import itertools as it
from typing import Iterable, Iterator, Literal

test_inputs = [('example', '''\
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop\
''', [
	('sigstrengths_csv', '420,1140,1800,2940,2880,3960'),
	('p1', '13140'),
	('p2', '''\
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....\
''')
])]

Instr = tuple[Literal['noop']] | tuple[Literal['addx'], int]

def parse(ip) -> Iterator[Instr]:
	for line in ip.splitlines():
		parts = line.split()

		if len(parts) == 1:
			assert parts[0] == 'noop'
			yield 'noop',
		elif len(parts) == 2:
			assert parts[0] == 'addx'
			yield 'addx', int(parts[1])
		else:
			assert False

def run(prog: Iterable[Instr]) -> Iterator[int]: # iterator of register values
	reg = 1

	for instr in prog:
		if instr[0] == 'noop':
			yield reg
		elif instr[0] == 'addx':
			v = instr[1]
			yield reg
			yield reg
			reg += v
		else:
			print(instr)
			assert False

def states(prog: Iterable[Instr]) -> Iterator[tuple[int, int]]: # iterator of (cycle number, register value) tuples
	return enumerate(run(prog), start=1)

def sigstrengths(ip: str) -> Iterator[int]:
	for cycle, reg in states(parse(ip)):
		if (cycle - 20) % 40 == 0:
			yield cycle * reg

def sigstrengths_csv(ip: str) -> str:
	return ','.join(map(str, sigstrengths(ip)))

def p1(ip: str) -> int:
	return sum(sigstrengths(ip))

CRT_W = 40
CRT_H = 6

def screen(ip: str) -> int:
	rows = [[None] * CRT_W for _ in range(CRT_H)]

	for cycle, reg in states(parse(ip)):
		rownum, colnum = divmod((cycle - 1), CRT_W)
		rows[rownum][colnum] = '#' if abs(colnum - reg) <= 1 else '.'

	return rows

def p2(ip: str) -> str:
	rows = screen(ip)
	return '\n'.join(''.join(row) for row in rows)