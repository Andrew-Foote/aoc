from dataclasses import dataclass
from enum import Enum
import math
from typing import Callable, Iterator, Optional

test_inputs = [
	('example', '''\
Hit Points: 12
Damage: 7
Armor: 2
''', [
		('example_battle_trace', '9,6,6,4,3,2,0'),
		('example_battle_result', '1'),
	])
]

# wait this is actually 2015

# turn based battle, player vs boss
# player goes first
# attack causes >= 1 damage
# first to get to 0 hp loses
# damage dealt = 
# max( (attacker damage score) - (defender armor score), 1 )
# player hp = 100
# player damage score is the sum of their items' damage scores
# likewise for armor
# you have to wear exactly one weapon, at most one piece of armor,
# and at most 2 rings. Rings are unique

@dataclass
class Actor:
	hp: int
	dmg: int
	arm: int

class ItemSort(Enum):
	WEAPON = 0
	ARMOR = 1
	RING = 2

@dataclass
class Item:
	sort: ItemSort
	name: str
	cost: int
	dmg: int
	arm: int

def item_cost(item: Optional[Item]) -> int:
	return 0 if item is None else item.cost

def item_dmg(item: Optional[Item]) -> int:
	return 0 if item is None else item.dmg

def item_arm(item: Optional[Item]) -> int:
	return 0 if item is None else item.arm

WEAPONS = [Item(ItemSort.WEAPON, name, cost, dmg, arm) for name, cost, dmg, arm in (
	('Dagger', 8, 4, 0),
	('Shortsword', 10, 5, 0),
	('Warhammer', 25, 6, 0),
	('Longsword', 40, 7, 0),
	('Greataxe', 74, 8, 0)
)]

ARMORS = [Item(ItemSort.ARMOR, name, cost, dmg, arm) for name, cost, dmg, arm in (
	('Leather', 13, 0, 1),
	('Chainmail', 31, 0, 2),
	('Splintmail', 53, 0, 3),
	('Bandedmail', 75, 0, 4),
	('Platemail', 102, 0, 5),
)]

RINGS = [Item(ItemSort.RING, name, cost, dmg, arm) for name, cost, dmg, arm in (
	('Damage +1', 25, 1, 0),
	('Damage +2', 50, 2, 0),
	('Damage +3', 100, 3, 0),
	('Defense +1', 20, 0, 1),
	('Defense +2', 40, 0, 2),
	('Defense +3', 80, 0, 3),
)]

def damage_dealt(attacker: Actor, defender: Actor) -> int:
	return max(attacker.dmg - defender.arm, 1)

def battle_trace(player: Actor, enemy: Actor) -> list[int]:
	actors = [player, enemy]
	hps = [player.hp, enemy.hp]
	trace = []
	turn = 0

	while hps[turn] > 0:
		hps[1 - turn] -= actors[turn].dmg - actors[1 - turn].arm
		trace.append(hps[1 - turn])
		turn = 1 - turn

	return trace

def does_player_win(player: Actor, enemy: Actor) -> bool:
	pdd = damage_dealt(player, enemy)
	edd = damage_dealt(enemy, player)
	# each turn, player deals pdd damage and enemy deals edd damage
	# so in n rounds, player hp is down by n * edd, enemy hp is down by n * pdd
	# player hp(t) = player.hp - t * edd
	# enemy hp(t) = enemy.hp - t * edd
	# player wins if hp(t) reaches 0 at a smaller or equal t than enemy hp(t)
	# php(t) <= 0 iff player.hp - t * edd <= 0
	# i.e. player.hp/edd <= t
	# ehp(t) <= 0 iff enemy.hp - t * pdd <= 0
	# i.e. enemy.hp/pdd <= t
	# the game ends when EITHER (a) t >= player.hp/edd OR (b) t >= enemy.hp/pdd
	# if (a) holds, player wins (even if (b) holds too), if only (b) holds then enemy wins
	# so player wins as long as the smallest possible t satisfying (a) (which is math.ceil(player.hp/edd))
	# is less than or equal to the smallest possible t satisfying (b) (math.ceil(enemy.hp/pdd))
	b = math.ceil(enemy.hp / pdd) <= math.ceil(player.hp / edd)
	trace = battle_trace(player, enemy)
	c = len(trace) % 2 == 1
	assert b == c, (b, c, player, enemy, edd, pdd, trace)
	return b

def parse(ip: str) -> Actor:
	lines = ip.splitlines()
	hp, dmg, arm = [int(line.split(':')[1].strip()) for line in lines]
	return Actor(hp, dmg, arm)

EXAMPLE_PLAYER = Actor(8, 5, 5)

def example_battle_trace(ip: str) -> int:
	return ','.join(map(str, battle_trace(EXAMPLE_PLAYER, parse(ip))))

def example_battle_result(ip: str) -> int:
	return int(does_player_win(EXAMPLE_PLAYER, parse(ip)))

class Attr(Enum):
	COST = 'cost'
	DAMAGE = 'damage'
	ARMOR = 'armor'

	@property
	def getter(self) -> Callable[[Optional[Item]], int]:
		return {
			Attr.COST: item_cost,
			Attr.DAMAGE: item_dmg,
			Attr.ARMOR: item_arm,
		}[self]

class Slot(Enum):
	WEAPON = 'weapon'
	ARMOR = 'armor'
	RING1 = 'ring1'
	RING2 = 'ring2'

def possible_itemizations() -> Iterator[dict[Slot, Optional[Item]]]:
	for weapon in WEAPONS:
		for armor in ARMORS + [None]:
			rings = RINGS + [None]

			for i, ring1 in enumerate(rings):
				for ring2 in rings[i + 1:]:
					yield {
							Slot.WEAPON: weapon,
							Slot.ARMOR: armor,
							Slot.RING1: ring1,
							Slot.RING2: ring2
						}

			yield {Slot.WEAPON: weapon, Slot.ARMOR: armor, Slot.RING1: None, Slot.RING2: None}

def p1(ip: str) -> int:
	boss = parse(ip)
	costs = []

	for itemization in possible_itemizations():
		player = Actor(
			100,
			sum(Attr.DAMAGE.getter(itemization[slot]) for slot in Slot),
			sum(Attr.ARMOR.getter(itemization[slot]) for slot in Slot)
		)

		wins = does_player_win(player, boss)
		
		if wins:
			cost = sum(Attr.COST.getter(itemization[slot]) for slot in Slot)
			costs.append(cost)

	return min(costs)

def p2(ip: str) -> int:
	boss = parse(ip)
	costs = []

	for itemization in possible_itemizations():
		player = Actor(
			100,
			sum(Attr.DAMAGE.getter(itemization[slot]) for slot in Slot),
			sum(Attr.ARMOR.getter(itemization[slot]) for slot in Slot)
		)

		wins = does_player_win(player, boss)
		
		if not wins:
			cost = sum(Attr.COST.getter(itemization[slot]) for slot in Slot)
			costs.append(cost)

	return max(costs)
