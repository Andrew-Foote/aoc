from typing import Iterable, Iterator
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import NESW, Rect

test_inputs = [('example', '''\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2\
''', [
	('p1', '13'),
	('p2', '1')
]), ('example2', '''\
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20\
''', [
	('p2', '36')
])]

DIRS = {'URDL'[i]: NESW[i] for i in range(4)}

def parse(ip: str) -> Iterator[tuple[gint, int]]:
	for line in ip.splitlines():
		dir_s, count = line.split(' ')
		yield DIRS[dir_s], int(count)

def step(n: int, moves: Iterable[tuple[gint, int]]) -> Iterator[list[gint]]:
	knotpos = [gint()] * n
	yield knotpos

	for d0, count in moves:
		for _  in range(count):
			d = d0

			# print(diagram(n, knotpos))
			# input()

			for i, pos in enumerate(knotpos):
				knotpos[i] = pos + d

				if i < len(knotpos) - 1:
					dif = knotpos[i] - knotpos[i + 1]
					yunit = dif.imag // abs(dif.imag) if dif.imag else 0
					xunit = dif.real // abs(dif.real) if dif.real else 0
					d = gint(xunit, yunit) if abs(dif.real) > 1 or abs(dif.imag) > 1 else gint()

			yield knotpos

def p1(ip: str) -> int:
	return len({knotpos[-1] for knotpos in step(2, parse(ip))})

def p2(ip: str) -> int:
	return len({knotpos[-1] for knotpos in step(10, parse(ip))})

def diagram(n: int, knotpos: list[gint]) -> str:
	rect = Rect.bounding((gint(), *knotpos))
	lines = ['---']

	for y in range(rect.top, rect.bottom + 1):
		chars = ['|']
		for x in range(rect.left, rect.right + 1):

			for k, z in enumerate(knotpos):
				if z == gint(x, y):
					knot_s = str(k)
					if knot_s == '0': knot_s = 'H'
					if knot_s == str(n - 1): knot_s = 'T'
					chars.append(knot_s)
					break
			else:
				chars.append('.')

			chars.append('|')

		lines.append(''.join(chars))

	lines.append('---')

	return '\n'.join(lines)

if __name__ == '__main__':
	n = 10

	import sys

	with open('input/2022/9.txt') as f:
		ip = f.read()

	moves = parse(ip)
	iterator = step(n, parse(ip))
	knotpos = next(iterator)

	rect = Rect(-60, 79, 59, -80)

	import numpy as np
	import sdl2
	import sdl2.ext
	import sdl2.ext.pixelaccess

	sdl2.ext.init()
	window = sdl2.ext.Window('Advent of Code Day 9 Animation', size=(rect.width * 5, rect.height * 5))

	surface = window.get_surface()
	view = sdl2.ext.pixelaccess.pixels2d(surface)

	def knot_color(z, hpos):
		for k, w in enumerate(knotpos):
			if w == z + hpos:
				brightness = 256 - k * 16
				color = brightness
				return color

		# the middle of the screen will be hpos
		# distance from 0
		# n = int(abs(z + hpos))
		# brightness = n // 4
		# return brightness << 16 | brightness << 8 | brightness

		if z + hpos == gint(10, 10):
			return 0xff_00_00
		else:
			return 0

	def knotpos_as_array():
		hpos = knotpos[0]

		return np.block([[
			np.full((5, 5), knot_color(gint(x, y), hpos), dtype='uint32')
			for y in range(rect.top, rect.bottom + 1)
		] for x in range(rect.left, rect.right + 1)])

	def do_update():
		np.copyto(view, knotpos_as_array())

	do_update()

	window.show()

	last_update_ticks = sdl2.SDL_GetTicks()
	ticks = None
	exhausted = False

	TICKS_PER_UPDATE = 1
	STEPS_PER_UPDATE = 1

	while True:
		events = sdl2.ext.get_events()

		for event in events:
			if event.type == sdl2.SDL_QUIT:
				sys.exit()

		if not exhausted:
			ticks = sdl2.SDL_GetTicks()

			if ticks - last_update_ticks > TICKS_PER_UPDATE:
				for _ in range(STEPS_PER_UPDATE):
					try:
						knotpos = next(iterator)
					except StopIteration:
						exhausted = True
						break

				do_update()
				last_update_ticks = ticks

		window.refresh()
