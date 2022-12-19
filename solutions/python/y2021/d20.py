from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from enum import Enum
from typing import Self
from solutions.python.lib.digits import int_from_digits_leading_first
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, Rect

test_inputs = [('example', '''\
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###\
''', [
	('step_pics', '''\
#..#.
#....
##..#
..#..
..###

.##.##.
#..#.#.
##.#..#
####..#
.#..##.
..##..#
...#.#.

.......#.
.#..#.#..
#.#...###
#...##.#.
#.....#.#
.#.#####.
..#.#####
...##.##.
....###..\
'''),
	('p1', 35),
	('p2', 3351)
])]

class Color(Enum):
	LIGHT = '#'
	DARK = '.'

Alg = list[Color]

@dataclass
class Img:
	default: Color
	entries: dict[gint, Color]

	def __getitem__(self: Self, z: gint) -> Color:
		return self.entries.get(z, self.default)

	def rect(self: Self) -> Rect:
		return Rect.bounding(self.entries.keys())

	def picture(self: Self) -> str:
		return self.rect().picture(lambda z: self[z].value)

def parse(ip: str) -> list[Alg, Img]:
	alg_s, img_s = ip.split('\n\n')
	alg_s = alg_s.replace('\n', '')
	assert len(alg_s) == 512
	alg = list(map(Color, alg_s))

	img_lines = img_s.splitlines()
	img_height = len(img_lines)
	img_width = len(img_lines[0])
	assert all(len(line) == img_width for line in img_lines[1:])

	img = Img(Color.DARK, {
		gint(x, y): Color(img_lines[y][x])
		for y in range(img_height) for x in range(img_width)
	})

	return alg, img

def output_img(alg: Alg, img: Img) -> Img:
	if img.default == Color.DARK:
		new_default = alg[0]
	elif img.default == Color.LIGHT:
		new_default = alg[511]
	else:
		assert False

	bounding_rect = img.rect()
	
	rect = Rect.bounding((
		bounding_rect.top_left - gint(1, 1),
		bounding_rect.bottom_right + gint(1, 1)
	))
	
	new_entries = {}

	for z in rect:
		subrect = Rect.bounding((z - gint(1, 1), z + gint(1, 1)))
		bits = [img[w] == Color.LIGHT for w in subrect]
		number = int_from_digits_leading_first(bits, 2)
		assert 0 <= number < 512
		new_entries[z] = alg[number]

	return Img(new_default, new_entries)

def step(ip: str) -> Iterator[Img]:
	alg, img = parse(ip)
	yield img

	while True:
		img = output_img(alg, img)
		yield img

def step_pics(ip: str) -> str:
	imgs = step(ip)
	result = []

	for i, img in enumerate(step(ip)):
		result.append(img.picture())

		if i == 2:
			break

	# print()
	# print('\n\n'.join(result))
	# print('---')
	return '\n\n'.join(result)

def p1(ip: str) -> int:
	imgs = step(ip)

	for i, img in enumerate(step(ip)):
		# print()
		# print(Rect.bounding(img).picture(lambda z: '#' if z in img else '.'))
		# print()

		if i == 2:
			return sum(1 for z, color in img.entries.items() if color == Color.LIGHT)

def p2(ip: str) -> int:
	imgs = step(ip)

	for i, img in enumerate(step(ip)):
		# print()
		# print(Rect.bounding(img).picture(lambda z: '#' if z in img else '.'))
		# print()

		if i == 50:
			return sum(1 for z, color in img.entries.items() if color == Color.LIGHT)


# 5255
# 5761