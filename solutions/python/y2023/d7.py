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
	('p1','6440')
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

