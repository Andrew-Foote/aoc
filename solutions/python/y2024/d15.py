from enum import Enum
import itertools as it
from typing import assert_never, Self
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import EAST, Grid, NORTH, SOUTH, WEST

test_inputs = [
    ('example', '''\
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^''', [
        ('p1', 10092),
    ]), ('example2', '''\
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<''', [
        ('p1', 2028)
    ])
]

class Cell(Enum):
    CLEAR = '.'
    BOX = 'O'
    WALL = '#'

def parse_dir(s: str) -> gint:
    match s:
        case '^':
            return NORTH
        case '>':
            return EAST
        case 'v':
            return SOUTH
        case '<':
            return WEST
        case _:
            raise ValueError(f"unrecognized direction character: '{s}'")

def parse(ip: str) -> tuple[Grid, gint, list[gint]]:
    # robot is @
    # . is clear space
    # O is box
    # # is wall
    # need the sum of 100 * x + y for all positions (x, y) of boxes, after the
    # robot finishes moving

    grid_s, moves_s = ip.split('\n\n')
    robot_pos: gint | None = None

    def parse_grid_char(pos: gint, char: str) -> Cell:
        nonlocal robot_pos

        match char:
            case '@':
                robot_pos = pos
                return Cell.CLEAR
            case '.':
                return Cell.CLEAR
            case 'O':
                return Cell.BOX
            case '#':
                return Cell.WALL
            case _:
                raise ValueError(f"unrecognized grid character: '{char}'")

    grid = Grid.parse(grid_s.splitlines(), parse_grid_char)
    assert robot_pos is not None
    moves = [parse_dir(d) for d in moves_s.replace('\n', '')]
    return grid, robot_pos, moves

def move_robot(grid: Grid[Cell], robot_pos: gint, move: gint) -> gint:
    # print(f'move_robot(grid, {robot_pos}, {move})')
    assert grid[robot_pos] == Cell.CLEAR

    for i in it.count(1):
        target_pos = robot_pos + i * move
        # print(f'target_pos={target_pos}, grid[target_pos]={grid[target_pos]}')
        target_cell = grid[target_pos]

        match target_cell:
            case Cell.CLEAR:
                for j in range(i, 0, -1):
                    old_pos = robot_pos + (j - 1) * move
                    new_pos = robot_pos + j * move
                    grid[new_pos] = grid[old_pos]

                new_robot_pos = robot_pos + move
                break
            case Cell.BOX:
                continue
            case Cell.WALL:
                new_robot_pos = robot_pos
                break
            case _:
                assert_never(target_cell)

    return new_robot_pos

def p1(ip: str) -> int:
    grid, robot_pos, moves = parse(ip)
    
    for move in moves:
        robot_pos = move_robot(grid, robot_pos, move)

    result: int = 0

    for p in grid.rect():
        if grid[p] == Cell.BOX:
            result += p.real + p.imag * 100

    return result
