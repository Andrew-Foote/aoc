from collections.abc import Iterable, Iterator
from dataclasses import dataclass, field
import functools as ft
import itertools as it
import re
from typing import Self, Type, TypeVar
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW

test_inputs = [('example', '''\
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5\
''', [
    ('p1', 6032),
    ('p2', 0)
])]

Path = list[int | str]

def parse(ip: str) -> tuple[Grid[str], Path]:
    parts = ip.split('\n\n')
    board, path = parts
    boardrows = board.splitlines()
    boardmaxrowlen = max(len(row) for row in boardrows)

    for i, row in enumerate(boardrows):
        boardrows[i] += (boardmaxrowlen - len(row)) * ' '

    board_grid = Grid(boardrows)
    # print()
    # print(board_grid.rect().picture(lambda z: '~' if board_grid[z] == ' ' else board_grid[z]))
    path = path.strip()
    # print(path)

    pathelems = []
    curnum = ''

    for c in path:
        if c.isdigit():
            curnum += c
        elif c in ('L', 'R'):
            if curnum:
                pathelems.append(int(curnum))
                curnum = ''
            pathelems.append(c)
        else:
            assert False

    if curnum: pathelems.append(int(curnum))

    # print(pathelems)

    return board_grid, pathelems

def p1(ip: str) -> int:
    board, path = parse(ip)
    rect = board.rect()
    x0 = min(x for x in range(rect.width) if board[gint(x, 0)] == '.')
    pos = gint(x0, 0)
    diri = 1 # NESW index
             # 3012
    print(path)
    for instr in path:
        # print(f'{instr=}, {diri=}')
        # print(board.rect().picture(lambda z: 'P' if z == pos else '~' if board[z] == ' ' else board[z]))
        # input()

        try:
            n = int(instr)
        except ValueError:
            if instr == 'R':
                diri = (diri + 1) % 4
            elif instr == 'L':
                diri = (diri - 1) % 4
            else:
                assert False
        else:
            for _ in range(n):
                new_pos = pos + NESW[diri]
                new_pos = gint(new_pos.real % rect.width, new_pos.imag % rect.height)
                # print(f'from {pos} to {new_pos}')

                while board[new_pos] == ' ':
                    # print(f'advancing new_pos from {new_pos} cuz blank')
                    new_pos = new_pos + NESW[diri]
                    new_pos = gint(new_pos.real % rect.width, new_pos.imag % rect.height)

                if board[new_pos] == '#':
                    break
                else:
                    pos = new_pos

    rownum = pos.imag + 1
    colnum = pos.real + 1
    facing = (diri - 1) % 4
    # print(rownum, colnum, facing)
    return 1000 * rownum + 4 * colnum + facing

def p2(ip: str) -> int:
    return 0