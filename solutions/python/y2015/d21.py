from dataclasses import dataclass
from enum import Enum
from solutions.python.lib import linopt
from typing import Callable, Iterator, Optional

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

def does_player_win(player: Actor, enemy: Actor) -> bool:
	pdd = damage_dealt(player, enemy)
	edd = damage_dealt(enemy, player)
	# each turn, player deals pdd damage and enemy deals edd damage
	# so in n rounds, player hp is down by n * edd, enemy hp is down by n * pdd
	# game ends when n * edd exceeds player hp, or n * pdd exceeds enemy hp
	# n * edd >= php   OR   n * pdd >= ehp
	# n >= php/edd     OR   n >= ehp/pdd
	# so the game ends when n exceeds min(php/edd, ehp/pdd)
	# and player wins if ehp/pdd is smaller (or equal, since player goes first)
	# in other words if ehp/pdd <= php/edd
	# or equivalenlty ehp * edd <= php * pdd
	return enemy.hp * edd <= player.hp * pdd

def parse(ip: str) -> Actor:
	lines = ip.splitlines()
	hp, dmg, arm = [int(line.split(':')[1].strip()) for line in lines]
	print(hp, dmg, arm)
	return Actor(hp, dmg, arm)

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
			if cost == 91:
				print('---')
				print(itemization)
				print('cost:', cost)
				print('pdd:', damage_dealt(player,boss))
				print('pdd * php:', 100 * damage_dealt(player,boss))
				print('bdd * bhp:', boss.hp * damage_dealt(boss,player))

			costs.append(cost)

	return min(costs)

	# # gold spent = weapon cost + armor cost + ring 1 cost + ring 2 cost
	# # we have 5 weapons, 6 armors, 8 rings, so 5 * 6 * 8 = 240 combinations
	# # we can just check each one individually

	# for 
	# 	if slot == 

	# vs = {}

	# for slot in Slot:
	# 	for attr in Attr:
	# 		vs[slot, attr] = linopt.var(f'{slot} {attr}')

	# objective = sum(vs[slot, Attr.COST] for slot in Slot)
	# PLAYER_HP = 100
	# player_dmg = sum(vs[slot, Attr.DAMAGE] for slot in Slot)
	# player_arm = sum(vs[slot, Attr.ARMOR] for slot in Slot)
	
	# constraint_sets = [
	# 	[
	# 		linopt.ge(boss.dmg - player_arm, 1),
	# 		linopt.ge(player_dmg - boss.dmg, 1),
	# 		linopt.le(boss.hp * (boss.dmg - player_arm), PLAYER_HP * (player_dmg - boss.arm))
	# 	],
	# 	[
	# 		linopt.le(boss.dmg - player_arm, 1),
	# 		linopt.ge(player_dmg - boss.dmg, 1),
	# 		linopt.le(boss.hp, PLAYER_HP * (player_dmg - boss.arm))
	# 	],
	# 	[
	# 		linopt.ge(boss.dmg - player_arm, 1),
	# 		linopt.le(player_dmg - boss.dmg, 1),
	# 		linopt.le(boss.hp * (boss.dmg - player_arm), PLAYER_HP)
	# 	],
	# 	[
	# 		linopt.le(boss.dmg - player_arm, 1),
	# 		linopt.le(player_dmg - boss.dmg, 1),
	# 		linopt.le(boss.hp, PLAYER_HP)
	# 	]
	# ]

	# poss_results = []

	# for constraint_set in constraint_sets:
	# 	x, res = linopt.solve(objective, constraint_set)
	# 	print(x, res)
	# 	poss_results.append(res)

	# return min(poss_results)

