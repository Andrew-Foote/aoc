from collections import Counter
from dataclasses import dataclass
import functools as ft
from typing import Iterator, Self

test_inputs = [('example', '''\
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
''', [
	('ranks_csv','1,4,3,2,5'),
	('p1','6440'),
	('p2_ranks_csv', '1,3,2,5,4'),
	('p2','5905')
])]

CARDS = '23456789TJQKA'
CARD_TO_STRENGTH_MAP = {c: i for i, c in enumerate(CARDS)}

@dataclass(frozen=True)
class Card:
	label: str

	def strength(self: Self) -> int:
		return CARD_TO_STRENGTH_MAP[self.label]

	@ft.total_ordering
	def __lt__(self: Self, other: Self) -> bool:
		return self.strength() < other.strength()

Hand = tuple[Card, Card, Card, Card, Card]

def hand_from_str(s: str) -> Hand:
	return tuple(map(Card, s))

# the type of a hand corresponds to the multiset of cardinalities of items in the multiset
HandType = Counter[int]

def hand_type(hand: list[Card]) -> Hand:
	return Counter(Counter(hand).values())

HAND_TYPES = (
	Counter([1, 1, 1, 1, 1]),
	Counter([2, 1, 1, 1]),
	Counter([2, 2, 1]),
	Counter([3, 1, 1]),
	Counter([3, 2]),
	Counter([4, 1]),
	Counter([5]),
)

HAND_TYPE_TO_STRENGTH_MAP = {tuple(sorted(t.elements())): i for i, t in enumerate(HAND_TYPES)}

def hand_strength(hand: Hand) -> int:
	return HAND_TYPE_TO_STRENGTH_MAP[tuple(sorted(hand_type(hand).elements()))]

def hand_cmp(hand1: Hand, hand2: Hand) -> bool:
	str1, str2 = map(hand_strength, (hand1, hand2))

	if str1 < str2:
		return -1

	if str2 < str1:
		return 1

	for card1, card2 in zip(hand1, hand2):
		if card1 < card2:
			return -1

		if card2 < card1:
			return 1

	return 0

def parse(ip: str) -> Iterator[tuple[Hand, int]]:
	for line in ip.splitlines():
		hand, bid = line.split()
		yield hand_from_str(hand), int(bid)

def parse_ranks(ip: str) -> tuple[list[Hand], dict[Hand, int], dict[Hand, int]]:
	hands_with_bids = list(parse(ip))
	hands, _ = zip(*hands_with_bids)
	bid_map = dict(hands_with_bids)
	hands_sorted = sorted(hands, key=ft.cmp_to_key(hand_cmp))
	rank_map = {hand: i + 1 for i, hand in enumerate(hands_sorted)}
	return hands, bid_map, rank_map

def ranks_csv(ip: str) -> str:
	hands, bid_map, rank_map = parse_ranks(ip)
	return ','.join(str(rank_map[hand]) for hand in hands)

def p1(ip: str) -> int:
	hands, bid_map, rank_map = parse_ranks(ip)
	return sum(bid_map[hand] * rank_map[hand] for hand in hands)

P2_CARD_TO_STRENGTH_MAP = CARD_TO_STRENGTH_MAP | {'J' : -1}

def hand_str(hand: Hand) -> str:
	return ''.join(card.label for card in hand)

def effective_hand(hand: Hand) -> Hand:
	j_positions = set()
	hand_minus_js = []

	for i, card in enumerate(hand):
		if card.label == 'J':
			j_positions.add(i)
		else:
			hand_minus_js.append(card)

	if not hand_minus_js:
		return hand_from_str('AAAAA')

	#print(hand_str(hand))
	most_common = Counter(hand_minus_js).most_common()
	#print([(card.label, count) for hand, count in most_common])
	most_common_count = most_common[0][1]
	#print(most_common_count)
	most_common_cards = tuple(card for card, count in most_common if count == most_common_count)
	#print([card.label for card in most_common_cards])
	replacement_for_j = max(most_common_cards)
	#print(replacement_for_j.label)
	
	new_hand = tuple(
		(replacement_for_j if i in j_positions else card)
		for i, card in enumerate(hand)
	)

	#print(hand_str(new_hand))
	#print('---')
	return new_hand

def p2_hand_cmp(hand1: Hand, hand2: Hand) -> bool:
	str1, str2 = (hand_strength(effective_hand(h)) for h in (hand1, hand2))

	if str1 < str2:
		return -1

	if str2 < str1:
		return 1

	for card1, card2 in zip(hand1, hand2):
		if card1 < card2:
			return -1

		if card2 < card1:
			return 1

	return 0

def p2_parse_ranks(ip: str) -> tuple[list[Hand], dict[Hand, int], dict[Hand, int]]:
	hands_with_bids = list(parse(ip))
	hands, _ = zip(*hands_with_bids)
	bid_map = dict(hands_with_bids)
	hands_sorted = sorted(hands, key=ft.cmp_to_key(p2_hand_cmp))
	rank_map = {hand: i + 1 for i, hand in enumerate(hands_sorted)}
	return hands, bid_map, rank_map

def p2_ranks_csv(ip: str) -> str:
	hands, bid_map, rank_map = p2_parse_ranks(ip)
	return ','.join(str(rank_map[hand]) for hand in hands)

def p2(ip: str) -> int:
	hands, bid_map, rank_map = p2_parse_ranks(ip)
	return sum(bid_map[hand] * rank_map[hand] for hand in hands)
	# 250785169 too low (first attempt)
	# 253283591 too low (second attempt)
	# 253423744 too high (third attempt)