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

def p2(ip: str) -> int:
    board, path = parse(ip)
    rect = board.rect()


    x0 = min(x for x in range(rect.width) if board[gint(x, 0)] == '.')
    pos = gint(x0, 0)
    diri = 1 # NESW index
             # 3012

    # construct the 3d board, as a set of 3d points
    # identify the six faces as rects, and their adjacencies in the net
    # then go through each one and project the points onto 3d
    board3d = {}

    # we know our start position is on the top left edge of one of the faces.
    # we can then just measure the side length going right
    side_length = 0
    while pos + side_length in rect and board[pos + side_length] != ' ':
        side_length += 1

    root_face = Rect(0, x0 + side_length - 1, side_length - 1, x0)

    face_children = lambda face: tuple(
        (di, face + side_length * d) for di, d in enumerate(NESW)
        if face.top_left + side_length * d in rect and board[face.top_left + side_length * d] != ' '
    )



    # Projection of our 2D position into 3D space.
    # Initially (x, y) -> (x, y, 0)
    # We "turn" this when we transition between faces. There
    # are four types of turns:
    # Up: e.g. [(x, y) -> (x, y, 0)] => [(x, y) -> (x, 0, -y)]
    #  (1 0)    (1 0 )     (1 0  0)
    #  (0 1) -> (0 0 )     (0 0  1)  hmm not sure about ths row
    #  (0 0)    (0 -1)     (0 -1 0)
    # Right: e.g. [(x, y) -> (x, y, 0)] => [(x, y) -> (0, y, x)],
    #     (0 0 1)
    #     (0 1 0)
    #     (1 0 0)
    # Down: e.g. [(x, y) -> (x, y, 0)] => [(x, y) -> (x, 0, y)],
    #     (1 0 0)
    #     (0 0 1)
    #     (0 1 0)
    # Left: e.g. [(x, y) -> (x, y, 0)] => [(x, y) -> (0, y, -x)]
    #     (0  0 1)
    #     (0  1 0)
    #     (-1 0 0)
    # Each entry in the matrix will always be either 0, 1 or -1
    # 


    # up: (x, y, 0) -> (x, 0, y)
    # right: (x, y, 0) -> (side_length, y, x)
    # down: (x, y, 0) -> (x, side_length, y)
    # left: (x, y, 0) -> (0, y, x)

    proj = np.array([
        [1, 0],
        [0, 1],
        [0, 0]
    ])

    URDL = [
        np.array([
            [1, 0, 0],
            [0, 0, 1],
            [0, -1, 0]
        ]),
        np.array([
            [0, 0, 1],
            [0, 1, 0],
            [1, 0, 0]
        ]),
        np.array([
            [1, 0, 0],
            [0, 0, 1],
            [0, 1, 0],
        ]),
        np.array([
            [0, 0, 1],
            [0, 1, 0],
            [-1, 0, 0]
        ])
    ]

    stack = [(proj, root_face)]
    visited = set()

    while stack:
        proj, face = stack.pop()

        if face not in visited:
            print(face.top_left, face.bottom_right)
            print(face.picture(lambda z: '~' if board[z] == ' ' else board[z]))
            print()
            visited.add(face)

            for z in face:
                assert z in rect, (z, rect)
                z0 = z - face.top_left
                z3d = tuple(proj @ np.array([z0.real, z0.imag]))
                z3d = (
                    z3d[0] if z3d[0] >= 0 else side_length - z3d[0] - 1, 
                    z3d[1] if z3d[1] >= 0 else side_length - z3d[1] - 1, 
                    z3d[2] if z3d[2] >= 0 else side_length - z3d[2] - 1, 
                )
                print(z, z0, z3d, board[z])
                #assert z3d not in board3d, (z, z0, z3d)
                board3d[z3d] = board[z]

            children = face_children(face)

            for di, child in children:
                stack.append((URDL[di] @ proj, child))

    print(board3d)

    # assert len(board3d) == 6 * side_length ** 2 - 8 * side_length, (len(board3d), side_length)

    # # ok, now we should have the 3d board. let's do some asserts to make sure it was built correctly
    # for x, y, z in it.product(range(side_length), repeat=3):
    #     if x == 0 or x == side_length - 1 or y == 0 or y == side_length - 1 or z == 0 or z == side_length - 1:
    #         assert (x, y, z) in board3d, (x, y, z)
    #         assert board3d[x, y, z] in ('.', '#'), board3d[x, y, z]

    for x, y, z in board3d.keys():
        assert 0 <= x < side_length and 0 <= y < side_length and 0 <= z < side_length, (x, y, z)

    proj = np.array([
        [1, 0],
        [0, 1],
        [0, 0]
    ])

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

                if new_pos not in board.rect() or board[new_pos] == ' ':
                    pos3d = proj @ np.array((pos.real, pos.imag))


                    # find the next part of the cube to go to...
                    for cubename, cube in CUBES.items():
                        if pos in cube:
                            nextcube = CUBES[CUBE2CUBE[cubename][diri]]
                            new_pos = new_pos - cube.top_left + nextcube.top_left
                            print(new_pos)
                            break


                # new_pos = gint(new_pos.real % rect.width, new_pos.imag % rect.height)
                # print(f'from {pos} to {new_pos}')

                # if board[new_pos] == ' ':


                # while board[new_pos] == ' ':
                #     # print(f'advancing new_pos from {new_pos} cuz blank')
                #     new_pos = new_pos + NESW[diri]
                #     new_pos = gint(new_pos.real % rect.width, new_pos.imag % rect.height)

                if board[new_pos] == '#':
                    break
                else:
                    pos = new_pos

    rownum = pos.imag + 1
    colnum = pos.real + 1
    facing = (diri - 1) % 4
    # print(rownum, colnum, facing)
    return 1000 * rownum + 4 * colnum + facing
