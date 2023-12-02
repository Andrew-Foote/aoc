from dataclasses import dataclass
from enum import Enum
import re
from utils import joinlines
import typing

test_inputs = [('example', '''\
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\
''', [
	('parse_csv', '''\
1;4,0,3;1,2,6;0,2,0
2;0,2,1;1,3,4;0,1,1
3;20,8,6;4,13,5;1,5,0
4;3,1,6;6,3,0;14,3,15
5;6,3,1;1,2,2
'''),
	('possible_csv', '1,2,5'),
	('p1','8'),
	('requirements_csv', '4,2,6;1,3,4;20,13,6;14,3,15;6,3,2'),
	('p2','2286')
])]

class Color(Enum):
	RED = 'red'
	GREEN = 'green'
	BLUE = 'blue'

@dataclass
class Game:
	id_: int

	# red, green, blue
	reveals: list[dict[Color, int]]

	def is_possible(self, start: dict[Color, int]) -> bool:
		return all(
			reveal[color] <= start[color]
			for reveal in self.reveals for color in Color
		)

def parse_game(line: str) -> Game:
	m = re.match(r'Game (\d+): (.*)', line)
	if m is None: breakpoint()
	game_id = int(m.group(1))
	rest = m.group(2)
	reveal_strings = [rs.strip() for rs in rest.split(';')]
	reveals = []

	for reveal_string in reveal_strings:
		color_strings = [cs.strip() for cs in reveal_string.split(',')]
		colors = {}
		reveals.append(colors)

		for color_string in color_strings:
			m = re.match(r'(\d+) (\w+)', color_string)
			if m is None: breakpoint()
			amount, color_name = m.groups()
			colors[Color(color_name)] = int(amount)

		for color in Color:
			if color not in colors:
				colors[color] = 0

	return Game(game_id, reveals)

def parse_games(ip: str) -> typing.Iterator[Game]:
	for line in ip.splitlines():
		if line:
			yield parse_game(line)

def parse_csv(ip: str) -> str:
	games = parse_games(ip)
	oplines = []

	for game in games:
		oplines.append(';'.join([
			str(game.id_),
			*(
				','.join(str(reveal[color]) for color in (Color.RED, Color.GREEN, Color.BLUE))
				for reveal in game.reveals
			)
		]))

	return joinlines(oplines)

def possible_csv(ip: str) -> str:
	return ','.join(
		str(game.id_) for game in parse_games(ip)
		if game.is_possible({Color.RED: 12, Color.GREEN: 13, Color.BLUE: 14})
	)

def p1(ip: str) -> int:
	return sum(
		game.id_ for game in parse_games(ip)
		if game.is_possible({Color.RED: 12, Color.GREEN: 13, Color.BLUE: 14})
	)