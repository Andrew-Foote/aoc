from enum import Enum
import functools as ft
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
        ('p2_final_state_pic', '''\
####################
##[].......[].[][]##
##[]...........[].##
##[]........[][][]##
##[]......[]....[]##
##..##......[]....##
##..[]............##
##..@......[].[][]##
##......[][]..[]..##
####################'''),
        ('p2', 9021),
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
    ]), ('example3', '''\
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^''', [
        ('p2_state_pics', '''\
Initial state:
##############
##......##..##
##..........##
##....[][]@.##
##....[]....##
##..........##
##############

Move <:
##############
##......##..##
##..........##
##...[][]@..##
##....[]....##
##..........##
##############

Move v:
##############
##......##..##
##..........##
##...[][]...##
##....[].@..##
##..........##
##############

Move v:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.......@..##
##############

Move <:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##......@...##
##############

Move <:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.....@....##
##############

Move ^:
##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############

Move ^:
##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############

Move <:
##############
##......##..##
##...[][]...##
##....[]....##
##....@.....##
##..........##
##############

Move <:
##############
##......##..##
##...[][]...##
##....[]....##
##...@......##
##..........##
##############

Move ^:
##############
##......##..##
##...[][]...##
##...@[]....##
##..........##
##..........##
##############

Move ^:
##############
##...[].##..##
##...@.[]...##
##....[]....##
##..........##
##..........##
##############'''),
    ]), ('myexample', '''\
#####
#...#
#.O.#
#.O.#
#.@.#
#####

^''', [
        ('p2_state_pics', '''\
Initial state:
##########
##......##
##..[]..##
##..[]..##
##..@...##
##########

Move ^:
##########
##..[]..##
##..[]..##
##..@...##
##......##
##########'''),
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

def dir_pic(d: gint) -> str:
    match d.rect():
        case (0, -1):
            return '^'
        case (1, 0):
            return '>'
        case (0, 1):
            return 'v'
        case (-1, 0):
            return '<'
        case _:
            assert False

def parse(ip: str) -> tuple[Grid[Cell], gint, list[gint]]:
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

class P2Cell(Enum):
    ROBOT = -1
    CLEAR = 0
    BOX_L = 1
    BOX_R = 2
    WALL = 4

def p2_parse(ip: str) -> tuple[Grid[P2Cell], list[gint]]:
    grid_s, moves_s = ip.split('\n\n')
    rows: list[list[P2Cell]] = []

    for y, row_s in enumerate(grid_s.splitlines()):
        row: list[P2Cell] = []

        for x, cell_s in enumerate(row_s):
            match cell_s:
                case '@':
                    cell1 = P2Cell.ROBOT
                    cell2 = P2Cell.CLEAR
                case '.':
                    cell1 = cell2 = P2Cell.CLEAR
                case 'O':
                    cell1 = P2Cell.BOX_L
                    cell2 = P2Cell.BOX_R
                case '#':
                    cell1 = cell2 = P2Cell.WALL
                case _:
                    raise ValueError(f"unrecognized grid character: '{cell_s}'")

            row.extend((cell1, cell2))

        rows.append(row)

    grid = Grid(rows)
    moves = [parse_dir(d) for d in moves_s.replace('\n', '')]
    return grid, moves

def can_move(grid: Grid[P2Cell], obj_pos: gint, d: gint) -> bool:
    target_pos = obj_pos + d
    target_cell = grid[target_pos]

    match target_cell:
        case P2Cell.ROBOT:
            assert False
        case P2Cell.CLEAR:
            return True
        case P2Cell.WALL:
            return False
        case P2Cell.BOX_L:
            return (
                can_move(grid, target_pos, d)
                and can_move(grid, target_pos + EAST, d)
            )
        case P2Cell.BOX_R:
            return (
                can_move(grid, target_pos + WEST, d)
                and can_move(grid, target_pos, d)
            )

# maps a position of an object to be moved, to the MoveTree of all moves that
# have to be done before this object can be moved
type MoveTree = dict[gint, MoveTree]

def merge_move_trees(t1: MoveTree, t2: MoveTree) -> MoveTree:
    result = {}

    for t1_key, t1_val in t1.items():
        t2_val = t2.get(t1_key, {})
        result[t1_key] = merge_move_trees(t1_val, t2_val)

    for t2_key, t2_val in t2.items():
        if t2_key not in result:
            result[t2_key] = t2_val

    return result

indent = 0

def move_tree(grid: Grid[P2Cell], obj_pos: gint, d: gint) -> MoveTree:
    global indent
    # print(' ' * indent + f'move_tree(grid, {obj_pos}, {d})')
    # input()
    indent += 1
    target_pos = obj_pos + d
    target_cell = grid[target_pos]
    result: MoveTree

    match target_cell:
        case P2Cell.ROBOT:
            assert False
        case P2Cell.CLEAR:
            # print(' ' * indent + 'clear')
            result = {obj_pos: {}}
        case P2Cell.BOX_L:
            # print(' ' * indent + 'box_l')

            if d == EAST:
                deps = move_tree(grid, target_pos + EAST, d)
                if not deps: return {}
                result = {obj_pos: {target_pos: deps}}
            elif d in (NORTH, SOUTH):
                ldeps = move_tree(grid, target_pos, d)
                rdeps = move_tree(grid, target_pos + EAST, d)
                result = {obj_pos: merge_move_trees(ldeps, rdeps)} if ldeps and rdeps else {}
            elif d == WEST:
                # assert False            
                deps = move_tree(grid, target_pos, d)
                result = {obj_pos: deps} if deps else {}
            else:
                assert False, d
        case P2Cell.BOX_R:
            # print(' ' * indent + 'box_r')

            if d == WEST:
                deps = move_tree(grid, target_pos + WEST, d)
                if not deps: return {}
                result = {obj_pos: {target_pos: deps}}
            elif d in (NORTH, SOUTH):
                ldeps = move_tree(grid, target_pos + WEST, d)
                rdeps = move_tree(grid, target_pos, d)
                result = {obj_pos: merge_move_trees(ldeps, rdeps)} if ldeps and rdeps else {}
            elif d == EAST:
                # assert False
                deps = move_tree(grid, target_pos, d)
                result = {obj_pos: deps} if deps else {}
            else:
                assert False, d
        case P2Cell.WALL:
            # print(' ' * indent + 'wall')
            result = {}
        case _:
            assert_never(target_cell)

    # print(' ' * indent + f'move_tree(grid, {obj_pos}, {d}) = {result}')
    indent -= 1
    return result

def process_move_tree(grid: Grid[P2Cell], tree: MoveTree, d: gint, processed: set[gint] | None=None) -> None:
    if processed is None:
        processed = set()

    for obj_pos, deps in tree.items():
        process_move_tree(grid, deps, d, processed)

        if obj_pos not in processed:
            grid[obj_pos + d] = grid[obj_pos]
            grid[obj_pos] = P2Cell.CLEAR
            processed.add(obj_pos)
     
def p2_move_robot(grid: Grid[P2Cell], move: gint) -> None:
    # print(f'move_robot(grid, {robot_pos}, {move})')

    for p in grid.rect():
        if grid[p] == P2Cell.ROBOT:
            mvtree = move_tree(grid, p, move)
            process_move_tree(grid, mvtree, move)
            break
    else:
        raise RuntimeError('robot not found')

def p2_cell_pic(grid: Grid[P2Cell], pos: gint) -> str:
    cell = grid[pos]

    match cell:
        case P2Cell.ROBOT:
            return '@'
        case P2Cell.CLEAR:
            return '.'
        case P2Cell.BOX_L:
            return '['
        case P2Cell.BOX_R:
            return ']'
        case P2Cell.WALL:
            return '#'
        case _:
            assert_never(cell)

def p2_state_pic(grid: Grid[P2Cell]) -> str:
    return grid.rect().picture(ft.partial(p2_cell_pic, grid))

def p2_state_pics(ip: str) -> str:
    grid, moves = p2_parse(ip)
    initial_pic = 'Initial state:\n' + p2_state_pic(grid)
    pics = [initial_pic]

    for move in moves:
        p2_move_robot(grid, move)
        pics.append(f'Move {dir_pic(move)}:\n' + p2_state_pic(grid))

    return '\n\n'.join(pics)

def p2_final_state_pic(ip: str) -> Grid[P2Cell]:
    grid, moves = p2_parse(ip)

    for move in moves:
        p2_move_robot(grid, move)

    return grid.rect().picture(ft.partial(p2_cell_pic, grid))

def p2(ip: str) -> int:
    # with open('temp.txt', 'w') as f:
    #     f.write(p2_state_pics(ip))

    grid, moves = p2_parse(ip)

    for i, move in enumerate(moves):
        print(f'processing move {i}')
        p2_move_robot(grid, move)

    result: int = 0

    for p in grid.rect():
        if grid[p] == P2Cell.BOX_L:
            result += p.real + p.imag * 100

    return result
    # gives wrong answer for example input, is too slow on real input