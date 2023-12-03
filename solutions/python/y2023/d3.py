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
	('p2','16345'),
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
						print(adj, end = ' : ')
						if 0 <= adj[0] < width and 0 <= adj[1] < height:
							print(grid[adj[1]][adj[0]], end=' ; ')
							if grid[adj[1]][adj[0]] != '.':
								is_part_number = True
								break
						else:
							print('', end = ' ; ')

					if is_part_number:
						s += number

					print(number)
					number = None
					number_start = None
					number_len = None
					state = 'start'

	return s
