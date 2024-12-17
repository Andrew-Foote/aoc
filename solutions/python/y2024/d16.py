from collections.abc import Generator
from dataclasses import dataclass
import functools as ft
from typing import Self
from solutions.python.lib.gint import gint
from solutions.python.lib.graph import dijkstra
from solutions.python.lib.grid import EAST, Grid

test_inputs = [('example', '''\
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############''', [
    ('p1', 7036)
]), ('example2', '''\
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################''', [
    ('p1', 11048)
]), ('small-example', '''\
####
#SE#
####''', [
    ('p1', 1),
]), ('small-example-2', '''\
####
#ES#
####''', [
    ('p1', 2001)
])]

@ft.total_ordering
@dataclass(frozen=True)
class Node:
    cost: int
    pos: gint
    dir: gint

    def __hash__(self) -> bool:
        return hash((self.pos, self.dir))

    def __lt__(self, other: Self) -> bool:
        return self.cost < other.cost
    
    def __repr__(self) -> str:
        dirstring = {gint(0, -1): 'n', gint(0, 1): 's', gint(1, 0): 'e', gint(-1, 0): 'w'}
        return f'{self.pos}:{dirstring[self.dir]}'

def p1(ip: str) -> int:
    # S is start
    # E is end
    # each moment, thrree poss actions:
    #  - move forward (gains 1 point). not poss if wall in front
    #  - turn clockwise (gain 1000 points)
    #  - turn anticlockwise (gain 1000 points)
    # competition is for the lowest score
    # just simple dijsktra problem?

    grid = Grid(ip.splitlines())
    start_pos = next(p for p in grid.rect() if grid[p] == 'S')
    end_pos = next(p for p in grid.rect() if grid[p] == 'E')
    root = Node(0, start_pos, EAST)

    def children(node: Node) -> Generator[Node]:
        cost = node.cost
        pos = node.pos
        d = node.dir

        clockd = d * gint(0, 1)
        anticlockd = d * gint(0, -1)

        if pos + d in grid and grid[pos + d] != '#':
            # i = 1

            # while grid[pos + i * d + clockd] == '#' and grid[pos + i * d + anticlockd] == '#' and pos + i * d + d in grid and grid[pos + i * d + d] != '#':
            #     i += 1
            
            # yield Node(cost + i, pos + i * d, d)
            # yield Node(cost + 1, pos + d, d)
            yield Node(cost + 1 + abs(end_pos - pos) - abs(end_pos - (pos + d)), pos + d, d)

        if (pos + clockd in grid and grid[pos + clockd] != '#') or (pos - d in grid and grid[pos - d] != '#'):
            yield Node(cost + 1000, pos, d * gint(0, 1))

        if (pos + anticlockd in grid and grid[pos + anticlockd] != '#') or (pos - d in grid and grid[pos - d] != '#'):
            yield Node(cost + 1000, pos, d * gint(0, -1)) 

    for node in dijkstra(root, children):
        if grid[node.pos] == 'E':
            return node.cost
    else:
        assert False
