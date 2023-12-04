from dataclasses import dataclass
import re
import typing as t

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
	('instances_csv','1,2,4,8,14,1'),
	('p2','30')
])]

@dataclass
class Card:
	id_: int
	winning: set[int]
	have: set[int]

	@property
	def score(self) -> int:
		return int(2 ** (len(self.winning & self.have) - 1))

def parse_cards(ip: str) -> t.Iterator[Card]:
	for card in ip.splitlines():
		m = re.match(r'Card\s+(\d+):(.*)', card)
		if m is None: breakpoint()
		card_id, rest = m.groups()
		card_id = int(card_id)
		winning, have = [bit.strip() for bit in rest.split('|')]
		winning = { int(bit.strip()) for bit in winning.split() }
		have = { int(bit.strip()) for bit in have.split() }
		yield Card(card_id, winning, have)

def points_csv(ip: str) -> str:
	return ','.join(str(card.score) for card in parse_cards(ip))

def p1(ip: str) -> int:
	return sum(card.score for card in parse_cards(ip))

def parse_instances(ip: str) -> dict[int, int]:
	cards = list(parse_cards(ip))
	instances = {card.id_: 1 for card in cards}

	for i, card in enumerate(cards):
		mult = instances[card.id_]
		dist = len(card.winning & card.have)
		j = i + 1

		while j < len(cards) and j < i + 1 + dist:
			print('copying next', dist, 'cards: ', range(i + 1, i + 1 + dist))
			instances[cards[j].id_] += mult
			j += 1

	return instances

def instances_csv(ip: str) -> str:
	return ','.join(str(v) for _, v in sorted(parse_instances(ip).items(), key=lambda p: p[0]))

def p2(ip: str) -> int:
	instances = parse_instances(ip)
	return sum(instances.values())


