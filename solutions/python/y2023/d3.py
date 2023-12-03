from dataclasses import dataclass
from enum import Enum
import re
from utils import joinlines
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW, NE_SE_SW_NW
import typing

test_inputs = [('example', '''\
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
''', [
	('p1','4361'),
	('p2','467835'),
])]

def p1(ip: str) -> int:
	s = 0
	state = 'start'
	number = None
	number_start = None
	number_len = None

	grid = list(ip.splitlines())
	height = len(grid)
	width = len(grid[0])

	for i, line in enumerate(ip.splitlines()):
		for j, char in enumerate(line + '|'):
			if state == 'start':
				if char.isdigit():
					state = 'number'
					number = int(char)
					number_start = j, i
					number_len = 1
			elif state == 'number':
				if char.isdigit():
					number *= 10
					number += int(char)
					number_len += 1
				else:
					adjacents = [
						# above the number
						*((number_start[0] - 1 + k, number_start[1] - 1) for k in range(number_len + 2)),
						# right of the number
						(number_start[0] + number_len, number_start[1]),
						# below the number
						*reversed([(number_start[0] - 1 + k, number_start[1] + 1) for k in range(number_len + 2)]),
						# left of the number
						(number_start[0] - 1, number_start[1]),
					]

					is_part_number = False

					for adj in adjacents:
						if 0 <= adj[0] < width and 0 <= adj[1] < height:
							if grid[adj[1]][adj[0]] != '.':
								is_part_number = True
								break

					if is_part_number:
						s += number

					number = None
					number_start = None
					number_len = None
					state = 'start'

	return s

def p2(ip: str) -> int:
	state = 'start'
	number = None
	number_start = None
	number_len = None

	grid = list(ip.splitlines())
	height = len(grid)
	width = len(grid[0])

	part_numbers = []

	for i, line in enumerate(ip.splitlines()):
		for j, char in enumerate(line + '|'):
			if state == 'start':
				if char.isdigit():
					state = 'number'
					number = int(char)
					number_start = j, i
					number_len = 1
			elif state == 'number':
				if char.isdigit():
					number *= 10
					number += int(char)
					number_len += 1
				else:
					adjacents = [
						# above the number
						*((number_start[0] - 1 + k, number_start[1] - 1) for k in range(number_len + 2)),
						# right of the number
						(number_start[0] + number_len, number_start[1]),
						# below the number
						*reversed([(number_start[0] - 1 + k, number_start[1] + 1) for k in range(number_len + 2)]),
						# left of the number
						(number_start[0] - 1, number_start[1]),
					]

					is_part_number = False

					for adj in adjacents:
						if 0 <= adj[0] < width and 0 <= adj[1] < height:
							if grid[adj[1]][adj[0]] != '.':
								is_part_number = True
								break

					if is_part_number:
						part_numbers.append((number, number_start, number_len))

					number = None
					number_start = None
					number_len = None
					state = 'start'

	print(part_numbers)

	keyed_by_pos = {}

	for num_id, (number, (nx, ny), nlen) in enumerate(part_numbers):
		for k in range(nlen):
			keyed_by_pos[(nx + k, ny)] = num_id

	print(keyed_by_pos)

	s = 0

	for i, line in enumerate(ip.splitlines()):
		for j, char in enumerate(line + '|'):
			if char == '*':
				adjacents = [(j - 1, i - 1), (j - 1, i), (j - 1, i + 1), (j, i - 1), (j, i + 1), (j + 1, i - 1), (j + 1, i), (j + 1, i + 1)]
				adjnums = set()

				for adj in adjacents:
					if adj in keyed_by_pos:
						adjnums.add(keyed_by_pos[adj])

				if len(adjnums) == 2:
					n1, n2 = list(adjnums)
					gear_ratio = part_numbers[n1][0] * part_numbers[n2][0]
					print('GEAR AT ', i, j, 'GRAT=', gear_ratio)
					s += gear_ratio

	return s