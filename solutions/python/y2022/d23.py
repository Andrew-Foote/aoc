from collections import defaultdict
from collections.abc import Iterator
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import COMPASS, DefaultGrid

test_inputs = [('example', '''\
....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..\
''', [
	('p1', 0),
	('p2', 0)
])]

def parse(ip: str) -> DefaultGrid[str]:
	entries = {}

	for i, line in enumerate(ip.splitlines()):
		for j, char in enumerate(line):
			if char == '#':
				entries[gint(i, j)] = char

	return DefaultGrid(lambda z: '.', entries)

def adj(pos: gint) -> Iterator[gint]:
	for d in COMPASS:
		yield pos + d

DIRS: dict[str, gint] = {}

def round(grid: DefaultGrid[str]) -> None:
	proposals = {}
	elfposns = list(grid.entries.keys())
	posn_elves = defaultdict(lambda: [])

	# first half

	for elfpos in elfposns:
		haselfposns = {pos for pos in adj(elfpos) if pos in grid.entries}
		if not haselfposns: continue
		if {elfpos + DIRS[d] for d in ('N', 'NE', 'NW')}.isdisjoint(haselfposns):
			proposal = 'N'
		elif {elfpos + DIRS[d] for d in ('S', 'SE', 'SW')}.isdisjoint(haselfposns):
			proposal = 'S'
		elif {elfpos + DIRS[d] for d in ('W', 'NW', 'SW')}.isdisjoint(haselfposns):
			proposal = 'W'
		elif {elfpos + DIRS[d] for d in ('E', 'NE', 'SE')}.isdisjoint(haselfposns):
			proposal = 'E'

		proposals[elfpos] = elfpos + DIRS[proposal]
		posn_elves[proposals[elfpos]].append(elfpos)

	# second half

	for elfpos in elfposns:
		proposed_pos = proposals[elfpos]

		if len(posn_elves[proposed_pos]) <= 1:
			del grid.entries[elfpos]
			grid.entries[proposed_pos] = '#' # what if it's already occupied?


def p1(ip: str) -> int:
	grid = parse(ip)



	# for round_ in rounds:
	# 	# first half
	# 	for pos in adj():
	# 		pass

	# 	# second half
	return 0

def p2(ip: str) -> int:
	return 0