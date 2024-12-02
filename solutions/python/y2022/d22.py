from collections.abc import Iterable, Iterator
from dataclasses import dataclass, field
import functools as ft
import itertools as it
import re
from typing import Self, Type, TypeVar
import numpy as np
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW, Rect

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
    ('p2', 5031)
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

    pathelems: Path = []
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

CUBES = {
    'A': Rect.bounding(( gint(50, 0), gint(100, 50) )),
    'B': Rect.bounding(( gint(100, 0), gint(150, 50) )),
    'C': Rect.bounding(( gint(50, 50), gint(100, 100) )),
    'D': Rect.bounding(( gint(50, 100), gint(100, 150) )),
    'E': Rect.bounding(( gint(0, 100), gint(50, 150) )),
    'F': Rect.bounding(( gint(0, 150), gint(50, 200) )),
}

CUBE2CUBE = {
    'A': ('F', 'B', 'C', 'E'),
    'B': ('F', 'D', 'C', 'A'),
    'C': ('A', 'B', 'D', 'E'),
    'D': ('F', 'B', 'C', 'E'),
    'E': ('A', 'D', 'C', 'F'),
    'F': ('A', 'B', 'D', 'E')
}

# one side would be (x, y, )

def get_face_number_for_coords(coords: gint) -> int:
    if 50 <= coords.real < 100 and 0 <= coords.imag < 50:
        return 1
    elif 100 <= coords.real < 150 and 0 <= coords.imag < 50:
        return 2
    elif 50 <= coords.real < 100 and 50 <= coords.imag < 100:
        return 3
    elif 0 <= coords.real < 50 and 100 <= coords.imag < 150:
        return 4
    elif 50 <= coords.real < 100 and 100 <= coords.imag < 150:
        return 5
    elif 0 <= coords.real < 50 and 150 <= coords.imag < 200:
        return 6
    else:
        assert False


def p2(ip: str) -> int:
    board, path = parse(ip)
    rect = board.rect()

    # can't figure out how to fold an arbitrary net, so I'll just do the folding for my
    # specific input manually

    # Net looks like this:
    # 
    #   1122 
    #   33  
    # 4455 
    # 66   

    # i guess what we really need to know is the *edge* connections
    # and also the orientation of the edge connection
    # so like, the left edge of the 11 face connects to the top edge of the 44 facd
    # specifically, the bottom of the left edge of the 11 face connects to the right of the top edge of the 44 face
    # and the top of the left edge of the 11 face connects to the left of the top edge of the 44 face
    # if we were 3 away from the bottom of the left edge of the 11 face, and we want left
    # we would end up being 3 away from the right of the top edge of the 44 face
    # so i guess these connections are derivable from the corner connections
    # a corner connection says: this corner of this face connects to this corner of this other face
    # how do we identify corners? by face + ne/sw/nw/se

    # empty = nw
    # shaded = ne
    # crown = sw
    # circles = se

    # CORNERS = [
    #     { (1, DIR_NW), (4, DIR_NE), (6, DIR_NW) },
    #     { (1, DIR_NE), (2, DIR_NW), (6, DIR_SW) },
    #     { (1, DIR_SW), (4, DIR_NW), (3, DIR_NW) },
    #     { (1, DIR_SE), (2, DIR_SW), (3, DIR_NE) },
    #     { (5, DIR_NW), (4, DIR_NE), (3, DIR_SW) },
    #     { (5, DIR_NE), (3, DIR_SE), (2, DIR_SE) },
    #     { (5, DIR_SW), (4, DIR_SE), (6, DIR_NE) },
    #     { (5, DIR_SE), (6, DIR_SE), (2, DIR_NE) }
    # ]

    # when we leave the net on one side we need to know
    # (a) which face we are going to
    # (b) where we will be on this face
    #     we know we'll be on an edge
    #     and we'll be offset from a certain corner by an amount which
    #     is the same as the offset from one of the corners of the original
    #     edge
    #     
    # which folds to this...
    #     
    # nb: we need to know not only which face is which, but also which direction it is
    # facing, relative to the original direction in the net... imagine there's an arrow
    # pointing up on each face on the net, the directions below indicate which direction
    # that arrow is going after the folding   
    #   _______
    #  |\      |\      dotted side = front = 1, up
    #  | \     | \     right side = 2, up
    #  |  \    |  \    bottom side = 3, forward
    #  |   \___|___\   back side = 5, down
    #  |___|_._|....|  left side = 4, down
    #  \   |...\....|  top side = 6, left
    #   \  |....\...|   i think...
    #    \ |.....\..|
    #     \|......\.|
    #      \_______\|  

    NN, EE, SS, WW = NESW

    # this is what's given directly by the net. directions are relative to net orientation.
    # 
    # NEIGHBOURS = {
    #     1: {EE: 2, SS: 3},
    #     2: {WW: 1},
    #     3: {NN: 1, SS: 5},
    #     4: {EE: 5, SS: 6},
    #     5: {LL: 4, NN: 3},
    #     6: {NN: 4}
    # }

    # and here's what it expands to...
    NEIGHBOURS = {
        1: {EE: 2, SS: 3, NN: 6, WW: 4},
        2: {WW: 1, NN: 6, EE: 5, SS: 3},
        3: {NN: 1, SS: 5, WW: 4, EE: 2},
        4: {EE: 5, SS: 6, NN: 3, WW: 1},
        5: {WW: 4, NN: 3, EE: 2, SS: 6},
        6: {NN: 4, EE: 5, WW: 1, SS: 2}
    }

    x0 = min(x for x in range(rect.width) if board[gint(x, 0)] == '.')
    pos = gint(x0, 0)
    diri = 1 # NESW index
             # 3012
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
