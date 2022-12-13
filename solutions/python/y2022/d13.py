from typing import Iterator

test_inputs = [('example', '''\
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]\
''', [
	('ro_pair_indices_csv', '1,2,4,6'),
	('p1', '13'),
	('p2', '0')
])]

Packet = int | list['Packet']

def parse(ip) -> Iterator[tuple[Packet, Packet]]:
	for block in ip.split('\n\n'):
		packets = block.splitlines()
		packets = [eval(packet) for packet in packets]
		yield packets

def cmp(packet1, packet2):
	if isinstance(packet1, int):
		if isinstance(packet2, int):
			if packet1 < packet2:
				return -1

			if packet1 > packet2:
				return 1

			return 0

		return cmp([packet1], packet2)
	
	if isinstance(packet2, int):
		return cmp(packet1, [packet2])

	i = 0

	while i < len(packet1):
		if i >= len(packet2):
			return 1

		u = packet1[i]
		v = packet2[i]
		c = cmp(u, v)

		if c != 0:
			return c

		i += 1

	if i == len(packet2):
		return 0

	return -1

def ro_pair_indices(ip: str) -> int:
	for i, (packet1, packet2) in enumerate(parse(ip), start=1):
		if cmp(packet1, packet2) == -1:
			#print(i, ':', packet1, '<=', packet2)
			yield i

def ro_pair_indices_csv(ip: str) -> str:
	return ','.join(map(str, ro_pair_indices(ip)))

def p1(ip: str) -> int:
	return sum(ro_pair_indices(ip))

def p2(ip: str) -> int:
	return 0