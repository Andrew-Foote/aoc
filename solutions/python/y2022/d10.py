import itertools as it
from typing import Iterable, Iterator

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

REG_START = 1

def addx(reg, v):
	# wait two cycles?
	return reg + v

def noop(reg):
	# wait one cycle
	pass

def parse(ip) -> Iterator[tuple]:
	for line in ip.splitlines():
		parts = line.split()

		if len(parts) == 1: # noop
			yield parts # 'noop'
		elif len(parts) == 2:
			iname, v = parts # iname = 'addx'
			yield iname, int(v)
		else:
			assert False

def run(prog: Iterable[tuple]) -> Iterator[int]:
	reg = REG_START

	for instr in prog:
		if instr[0] == 'noop':
			yield reg
		elif instr[0] == 'addx':
			v = instr[1]
			yield reg
			yield reg
			reg += v

def sigstrengths(ip: str) -> Iterator[int]:
	prog = parse(ip)
	regs = run(prog)

	for cycle, reg in enumerate(regs, start=1):
		if (cycle - 20) % 40 == 0:
			yield cycle * reg

def sigstrengths_csv(ip: str) -> str:
	return ','.join(map(str, sigstrengths(ip)))

def p1(ip: str) -> int:
	return sum(sigstrengths(ip))

# height, width
CRT_SIZE = (6, 40)

def p2(ip: str) -> int:
	states = run(parse(ip))
	screen = [[None] * 40 for _ in range(6)]

	for cycle, reg in enumerate(states, start=1):
		rownum, colnum = divmod((cycle - 1), 40)

		if reg - 1 <= colnum <= reg + 1:
			screen[rownum][colnum] = '#'
		else:
			screen[rownum][colnum] = '.'

	lines = []

	for row in screen:
		line = []

		for col in row:
			line.append(col)

		lines.append(''.join(line))

	print('\n'.join(lines))
	return '\n'.join(lines)