from dataclasses import dataclass
from enum import Enum
from solutions.python.lib import linopt

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

WEAPONS = [
	('Dagger', 8, 4, 0),
	('Shortsword', 10, 5, 0),
	('Warhammer', 25, 6, 0),
	('Longsword', 40, 7, 0),
	('Greataxe', 74, 8, 0)
]

ARMOR = [
	('Leather', 13, 0, 1),
	('Chainmail', 31, 0, 2),
	('Splintmail', 53, 0, 3),
	('Bandedmail', 75, 0, 4),
	('Platemail', 102, 0, 5),
	('No Armor', 0, 0, 0),
]

RINGS = [
	('Damage +1', 25, 1, 0),
	('Damage +2', 50, 2, 0),
	('Damage +3', 100, 3, 0),
	('Defense +1', 20, 0, 1),
	('Defense +2', 40, 0, 2),
	('Defense +3', 80, 0, 3),
	('No Ring in Slot 1', 0, 0, 0),
	('No Ring in Slot 2', 0, 0, 0),
]

ITEMS = [
	*(Item(ItemSort.WEAPON, name, cost, dmg, arm) for name, cost, dmg, arm in WEAPONS),
	*(Item(ItemSort.ARMOR, name, cost, dmg, arm) for name, cost, dmg, arm in ARMOR),
	*(Item(ItemSort.RING, name, cost, dmg, arm) for name, cost, dmg, arm in RINGS)
]

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
	print(lines)
	hp, dmg, arm = [line.split(':')[1].strip() for line in lines]
	return Actor(hp, dmg, arm)

class Attr(Enum):
	COST = 'cost'
	DAMAGE = 'damage'
	ARMOR = 'armor'

class Slot(Enum):
	WEAPON = 'weapon'
	ARMOR = 'armor'
	RING1 = 'ring1'
	RING2 = 'ring2'

def p1(ip: str) -> int:
	boss = parse(ip)
	vs = {}

	for slot in Slot:
		for attr in Attr:
			vs[slot, attr] = linopt.var(f'{slot} {attr}')

	objective = sum(vs[slot, Attr.COST] for slot in Slot)
	player_dmg = sum(vs[slot, Attr.DAMAGE] for slot in Slot)
	player_arm = sum(vs[slot, Attr.ARMOR] for slot in SLOT)
	
	constraint_sets = [
		[
			linopt.ge(boss.dmg - player_arm, 1),
			linopt.ge(player_dmg - boss.dmg, 1),
			linopt.le(boss.hp * (boss.dmg - player_arm), player.hp * (player_dmg - boss.arm))
		],
		[
			linopt.le(boss.dmg - player_arm, 1),
			linopt.ge(player_dmg - boss.dmg, 1),
			linopt.le(boss.hp, player.hp * (player_dmg - boss.arm))
		],
		[
			linopt.ge(boss.dmg - player_arm, 1),
			linopt.le(player_dmg - boss.dmg, 1),
			linopt.le(boss.hp * (boss.dmg - player_arm), player.hp)
		],
		[
			linopt.le(boss.dmg - player_arm, 1),
			linopt.le(player_dmg - boss.dmg, 1),
			linopt.le(boss.hp, player.hp)
		]
	]

	poss_results = []

	for constraint_set in constraint_sets:
		x, res = linopt.solve(objective, constraint_set)
		print(x, res)
		poss_results.append(res)

	return min(poss_results)

