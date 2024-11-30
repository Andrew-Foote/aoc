from dataclasses import dataclass
from enum import Enum
import functools as ft
from solutions.python.lib.gint import gint
from solutions.python.lib import graph
import solutions.python.lib.grid as g
from typing import Iterator, Optional, Self

test_inputs = [
	('example', '''\
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533''',
	[
		('path_pic', '''\
2>>34^>>>1323
32v>>>35v5623
32552456v>>54
3446585845v52
4546657867v>6
14385987984v4
44578769877v6
36378779796v>
465496798688v
456467998645v
12246868655<v
25465488877v5
43226746555v>'''),
		('p1', 102)
	])
]

def parse_as_grid(ip: str) -> g.Grid:
	return g.Grid(ip.splitlines(), conv=int)

class Dir(Enum):
	NORTH = 0
	EAST = 1
	SOUTH = 2
	WEST = 3

	def as_gint(self: Self) -> gint:
		return g.NESW[self.value]

	def as_ascii_arrow(self: Self) -> str:
		return DIR_AS_ASCII_ARROW[self]

	@classmethod
	def from_gint(self: Self, z: gint) -> Self:
		return NESW_R[z]

DIR_AS_ASCII_ARROW = {Dir.NORTH: '^', Dir.EAST: '>', Dir.SOUTH: 'v', Dir.WEST: '<'}
NESW_R: dict[gint, Dir] = {d.as_gint(): d for d in Dir}

@ft.total_ordering
@dataclass(frozen=True, slots=True)
class Node:
	cost: int
	pos: gint

	# dinfo = (d, n) where d is the current direction, and n is how many consecutive steps in this
	# direction it's taken to get to the current position. None for the start node
	dinfo: Optional[tuple[Dir, int]]

	@property
	def dir_(self: Self) -> Optional[Dir]:
		return None if self.dinfo is None else self.dinfo[0]

	def __lt__(self: Self, other: Self) -> bool:
		return (self.cost, self.pos.real, self.pos.imag) < (other.cost, other.pos.real, other.pos.imag)

	def children(self: Self, grid: g.Grid) -> Iterator['Node']:
		for d in g.NESW:
			new_pos = self.pos + d

			if new_pos not in grid.rect():
				continue

			if self.dinfo is None:
				count = 1
			else:
				cur, count = self.dinfo

				if d == cur:
					if count >= 3:
						continue

					count += 1
				else:
					count = 1

			yield Node(self.cost + grid[new_pos], new_pos, (Dir.from_gint(d), count))

def min_loss_path(grid: g.Grid) -> list[gint]:
	start = Node(0, grid.rect().top_left, None)
	path = []

	for node in graph.dijkstra(
		start,
		ft.partial(Node.children, grid=grid),
	):
		path.append(node)

		if node.pos == grid.rect().bottom_right + g.NW:
			break

	return path

def path_pic(ip: str) -> str:
	grid = parse_as_grid(ip)
	path = min_loss_path(grid)
	path_map = {node.pos: node.dir_.as_ascii_arrow() for node in path if node.dir_ is not None}
	pic = grid.rect().picture(lambda z: path_map[z] if z in path_map else str(grid[z]))
	print(pic)
	return pic

def p1(ip: str) -> int:
	grid = parse_as_grid(ip)
	path = min_loss_path(grid)
	return path[-1].cost	
