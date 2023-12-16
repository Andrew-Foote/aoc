
test_inputs = [('example', '''\
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
''', [
	('extrapolated_values_csv', '18,28,68'),
	('p1',114)
])]

def parse(ip: str) -> list[list[int]]:
	return [list(map(int, line.split())) for line in ip.splitlines()]

def extrapolate(history: list[int]) -> list[int]:
	diffslist = [history]

	while set(diffslist[-1]) != {0}:
		cur = diffslist[-1]
		diffslist.append([cur[i + 1] - cur[i] for i in range(len(cur) - 1)])

	for i in range(len(diffslist) - 1, -1, -1):
		if i == len(diffslist) - 1:
			diffslist[i].append(0)
		else:
			diffslist[i].append(diffslist[i][-1] + diffslist[i + 1][-1])

	return diffslist[0][-1]

def extrapolated_values_csv(ip: str) -> str:
	return ','.join(str(extrapolate(history)) for history in parse(ip))

def p1(ip: str) -> int:
	return sum(map(extrapolate, parse(ip)))