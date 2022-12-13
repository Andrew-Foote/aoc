import functools as ft
import itertools as it
from typing import Iterable, Iterator

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
	('sorted_example', '''\
[]
[[]]
[[[]]]
[1,1,3,1,1]
[1,1,5,1,1]
[[1],[2,3,4]]
[1,[2,[3,[4,[5,6,0]]]],8,9]
[1,[2,[3,[4,[5,6,7]]]],8,9]
[[1],4]
[[2]]
[3]
[[4,4],4,4]
[[4,4],4,4,4]
[[6]]
[7,7,7]
[7,7,7,7]
[[8,7,6]]
[9]\
'''),
	('divider_indices_csv', '10,14'),
	('p2', '140')
])]

Packet = int | list['Packet']

def packet_lex(s) -> Iterator[str | int]:
	digits = []

	def flush():
		if digits: yield int(''.join(digits))
		digits.clear()

	for c in s:
		if c in ('[', ']'):
			yield from flush()
			yield c
		elif c == ',':
			yield from flush()
		elif c.isdigit():
			digits.append(c)
		else:
			assert False

	yield from flush()

def packet_parse(toks: Iterable[str | int]) -> Packet:
	stack = [[]]

	for tok in toks:
		if tok == '[':
			stack.append([])
		elif tok == ']':
			assert len(stack) > 1, 'unmatched closing bracket'
			sub = stack.pop()
			stack[-1].append(sub)
		else:
			stack[-1].append(tok)

	assert len(stack) == 1, 'unmatched opening bracket'
	return stack[0][0]

def parse(ip) -> Iterator[tuple[Packet, Packet]]:
	for block in ip.split('\n\n'):
		packets = block.splitlines()
		packets = [packet_parse(packet_lex(packet)) for packet in packets]
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

def ro_pair_indices(ip: str) -> Iterator[int]:
	for i, (packet1, packet2) in enumerate(parse(ip), start=1):
		if cmp(packet1, packet2) == -1:
			#print(i, ':', packet1, '<=', packet2)
			yield i

def ro_pair_indices_csv(ip: str) -> str:
	return ','.join(map(str, ro_pair_indices(ip)))

def p1(ip: str) -> int:
	return sum(ro_pair_indices(ip))

DIVIDERS = ([[2]], [[6]])

def sorted_packets(ip: str) -> int:
	packets = list(it.chain.from_iterable(parse(ip)))
	packets.extend(DIVIDERS)	
	packets.sort(key=ft.cmp_to_key(cmp))
	return packets

def sorted_example(ip: str) -> str:
	packets = sorted_packets(ip)
	print(packets)
	return '\n'.join(str(packet).replace(' ', '') for packet in packets)

def divider_indices(ip: str) -> Iterator[int]:
	for i, packet in enumerate(sorted_packets(ip), start=1):
		if packet in DIVIDERS:
			yield i

def divider_indices_csv(ip: str) -> str:
	return ','.join(map(str, divider_indices(ip)))

def p2(ip: str) -> int:
	di1, di2 = divider_indices(ip)
	return di1 * di2