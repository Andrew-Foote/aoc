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
    ('p1', 7036),
    ('on_best_path_pic', '''\
###############
#.......#....O#
#.#.###.#.###O#
#.....#.#...#O#
#.###.#####.#O#
#.#.#.......#O#
#.#.#####.###O#
#..OOOOOOOOO#O#
###O#O#####O#O#
#OOO#O....#O#O#
#O#O#O###.#O#O#
#OOOOO#...#O#O#
#O###.#.#.#O#O#
#O..#.....#OOO#
###############'''),
    ('p2', 45),
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
    ('p1', 11048),
    ('on_best_path_pic', '''\
#################
#...#...#...#..O#
#.#.#.#.#.#.#.#O#
#.#.#.#...#...#O#
#.#.#.#.###.#.#O#
#OOO#.#.#.....#O#
#O#O#.#.#.#####O#
#O#O..#.#.#OOOOO#
#O#O#####.#O###O#
#O#O#..OOOOO#OOO#
#O#O###O#####O###
#O#O#OOO#..OOO#.#
#O#O#O#####O###.#
#O#O#OOOOOOO..#.#
#O#O#O#########.#
#O#OOO..........#
#################'''),
    ('p2', 64),
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

Node = tuple[gint, gint] # pos, dir

def p1(ip: str) -> int:
    grid = Grid(ip.splitlines())
    start_pos = next(p for p in grid.rect() if grid[p] == 'S')
    end_pos = next(p for p in grid.rect() if grid[p] == 'E')
    root = start_pos, EAST

    def children(node: Node) -> Generator[tuple[Node, int]]:
        pos, dir_ = node
        fw_pos = pos + dir_

        if fw_pos in grid and grid[fw_pos] != '#':
            yield (fw_pos, dir_), 1

        yield (pos, dir_ * gint(0, 1)), 1000
        yield (pos, dir_ * gint(0, -1)), 1000

    for dnode in dijkstra(root, children):
        pos, __ = dnode.node
        cost = dnode.cost

        if pos == end_pos:
            return cost

P2Node = tuple[Node, ...]

def points_on_best_paths(grid: Grid) -> set[gint]:
    start_pos = next(p for p in grid.rect() if grid[p] == 'S')
    end_pos = next(p for p in grid.rect() if grid[p] == 'E')
    root = (start_pos, EAST),

    def children(node: P2Node) -> Generator[tuple[P2Node, int]]:
        pos, dir_ = node[-1]
        fw_pos = pos + dir_

        if fw_pos in grid and grid[fw_pos] != '#':
            yield (*node, (fw_pos, dir_)), 1
        
        yield (*node, (pos, dir_ * gint(0, 1))), 1000
        yield (*node, (pos, dir_ * gint(0, -1))), 1000

    min_cost = None
    on_best_path: set[gint] = set()
    search = dijkstra(root, children)

    for dnode in search:
        pos, _ = dnode.node[-1]
        cost = dnode.cost

        if min_cost is None:
            if pos == end_pos:
                on_best_path.update(pos for pos, _ in dnode.node)
                min_cost = cost
        elif pos == end_pos:
            if cost <= min_cost:
                on_best_path.update(pos for pos, _ in dnode.node)
            else:
                break

    return on_best_path

def on_best_path_pic(ip: str) -> int:
    grid = Grid(ip.splitlines())
    on_best_path = points_on_best_paths(grid)

    def draw(p: gint) -> str:
        if p in on_best_path:
            return 'O'
        else:
            return grid[p]

    return grid.rect().picture(draw)

def p2(ip: str) -> int:
    grid = Grid(ip.splitlines())
    on_best_path = points_on_best_paths(grid)
    return len(on_best_path)