import re

test_inputs = [('example', '''\
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
''', [
	('points_csv','8,2,2,1,0,0'),
	('p1','13'),
])]

# winning on left
# have on right
# 2**(number of winning numbers you have) = points
# this is for each card
# we want the total points from each card

def parse_cards(ip: str) -> list[tuple[set[int], set[int]]]:
	cards = []

	for card in ip.splitlines():
		m = re.match(r'Card\s+\d+:(.*)', card)
		if m is None: breakpoint()
		rest, = m.groups()
		winning, have = [bit.strip() for bit in rest.split('|')]
		winning = { int(bit.strip()) for bit in winning.split() }
		have = { int(bit.strip()) for bit in have.split() }
		cards.append((winning, have))

	return cards

def points(winning: set[int], have: set[int]):
	count = len(winning & have)
	print(winning, have, winning & have)
	return int(2 ** (count - 1))

def points_csv(ip: str) -> str:
	return ','.join(str(points(winning, have)) for winning, have in parse_cards(ip))

def p1(ip: str) -> int:
	tot = 0

	for winning, have in parse_cards(ip):
		tot += points(winning, have)

	return tot