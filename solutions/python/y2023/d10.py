from enum import Enum
import itertools as it
from solutions.python.lib.gint import gint
import solutions.python.lib.grid as grid

test_inputs = [
#     ('example1', '''\
# .....
# .S-7.
# .|.|.
# .L-J.
# .....''', [
#         ('distance_map', '''\
# .....
# .012.
# .1.3.
# .234.
# .....'''),
#         ('p1', 4)
#     ]),
#     ('example2', '''\
# -L|F7
# 7S-7|
# L|7||
# -L-J|
# L|-JF''', [
#         ('main_loop_pic', '''\
# .....
# .S-7.
# .|.|.
# .L-J.
# .....''')
#     ]),
#     ('example3', '''\
# 7-F7-
# .FJ|7
# SJLL7
# |F--J
# LJ.LJ''', [
#         ('main_loop_pic', '''\
# ..F7.
# .FJ|.
# SJ.L7
# |F--J
# LJ...'''),
#         ('distance_map', '''\
# ..45.
# .236.
# 01.78
# 14567
# 23...'''),
#         ('p1', 8)
#     ]),
#     ('example4', '''\
# ...........
# .S-------7.
# .|F-----7|.
# .||.....||.
# .||.....||.
# .|L-7.F-J|.
# .|..|.|..|.
# .L--J.L--J.
# ...........''', [
#         ('p2', 4),
#         ('enclosed_map', '''\
# ...........
# .S-------7.
# .|F-----7|.
# .||.....||.
# .||.....||.
# .|L-7.F-J|.
# .|II|.|II|.
# .L--J.L--J.
# ...........''')
#     ]),
#     ('example5', '''\
# ..........
# .S------7.
# .|F----7|.
# .||....||.
# .||....||.
# .|L-7F-J|.
# .|..||..|.
# .L--JL--J.
# ..........''', [
#         ('p2', 4),
#         ('enclosed_map', '''\
# ..........
# .S------7.
# .|F----7|.
# .||....||.
# .||....||.
# .|L-7F-J|.
# .|II||II|.
# .L--JL--J.
# ..........''')
#     ]),
#     ('example6', '''\
# .F----7F7F7F7F-7....
# .|F--7||||||||FJ....
# .||.FJ||||||||L7....
# FJL7L7LJLJ||LJ.L-7..
# L--J.L7...LJS7F-7L7.
# ....F-J..F7FJ|L7L7L7
# ....L7.F7||L7|.L7L7|
# .....|FJLJ|FJ|F7|.LJ
# ....FJL-7.||.||||...
# ....L---J.LJ.LJLJ...''', [
#         ('enclosed_map', '''\
# .F----7F7F7F7F-7....
# .|F--7||||||||FJ....
# .||.FJ||||||||L7....
# FJL7L7LJLJ||LJIL-7..
# L--J.L7IIILJS7F-7L7.
# ....F-JIIF7FJ|L7L7L7
# ....L7IF7||L7|IL7L7|
# .....|FJLJ|FJ|F7|.LJ
# ....FJL-7.||.||||...
# ....L---J.LJ.LJLJ...
# '''),
#         ('p2', 8)
#     ])
]

PIPES = {
    '|': (grid.NORTH, grid.SOUTH),
    '-': (grid.EAST, grid.WEST),
    'L': (grid.NORTH, grid.EAST),
    'J': (grid.NORTH, grid.WEST),
    '7': (grid.SOUTH, grid.WEST),
    'F': (grid.SOUTH, grid.EAST),
}

HORIZONTALLY_CONNECTED_PIPE_PAIRS = {'--', '-J', '-7', 'L-', 'LJ', 'L7', 'F-', 'FJ', 'F7'}
VERTICALLY_CONNECTED_PIPE_PAIRS =   {'||', '|L', '|J', '7|', '7L', '7J', 'F|', 'FL', 'FJ'}

class Orient(Enum):
    HOZ = 0
    VER = 1

def pipes_are_opposite(p1: str, p2: str, hv: bool) -> bool:
    #print('pipes_are_opposite', p1, p2, end = ' ')

    if hv:
        res = grid.EAST in PIPES[p1] and grid.WEST in PIPES[p2]
        assert res == (p1 + p2 in HORIZONTALLY_CONNECTED_PIPE_PAIRS), (p1, p2, res)
    else:
        res = grid.SOUTH in PIPES[p1] and grid.NORTH in PIPES[p2]
        assert res == (p1 + p2 in VERTICALLY_CONNECTED_PIPE_PAIRS), (p1, p2, res)

    #print('hv', hv, 'res', res)
    return res

# . no pipe
# S start, plus pipe of unknown shape

# find the loop containing S
# and find the tile in that loop that is furthest away
# (measured in steps along the loop)

# s will always have exactly two connected pipes
# oh but we don't know what shape s is
# hm but may still apply

def parse(ip: str) -> grid.Grid[str]:
    rows = [line.strip() for line in ip.strip().splitlines()]
    return grid.Grid(rows)

def get_s_location(thegrid: grid.Grid[str]) -> gint:
    for z in thegrid.rect():
        if thegrid[z] == 'S':
            return z

def main_loop(thegrid: grid.Grid[str]) -> tuple[
    gint,
    list[tuple[gint, gint, int]],
    list[tuple[gint, gint, int]],
    tuple[gint, gint]
]:
    s_loc = get_s_location(thegrid)

    # find the two pipes connecting to S
    connectors = []

    for direction in grid.NESW:
        neighbour = s_loc + direction
        val = thegrid[neighbour]

        if val in PIPES:
            if -direction in PIPES[val]:
                connectors.append((neighbour, direction, 1))

    assert len(connectors) == 2
    steps1 = [connectors[0]]
    steps2 = [connectors[1]]
    sdirs = (connectors[0][1], connectors[1][1])

    # follow the path from each connector till they meet; this will be the point
    # of furthest distance

    cur1, cur2 = connectors

    while True:
        cur1_loc, cur1_dir, cur1_steps = cur1
        cur_pipe = PIPES[thegrid[cur1_loc]]
        next_dir = [next_dir for next_dir in cur_pipe if next_dir != -cur1_dir][0]
        next_loc = cur1_loc + next_dir
        assert thegrid[next_loc] in PIPES and -next_dir in PIPES[thegrid[next_loc]]
        cur1 = next_loc, next_dir, cur1_steps + 1
        steps1.append(cur1)

        cur2_loc, cur2_dir, cur2_steps = cur2
        cur_pipe = PIPES[thegrid[cur2_loc]]
        next_dir = [next_dir for next_dir in cur_pipe if next_dir != -cur2_dir][0]
        next_loc = cur2_loc + next_dir
        assert thegrid[next_loc] in PIPES and -next_dir in PIPES[thegrid[next_loc]]
        cur2 = next_loc, next_dir, cur2_steps + 1
        steps2.append(cur2)

        if cur1[0] == cur2[0]:
            assert cur1[2] == cur2[2]
            break

    return s_loc, steps1, steps2, sdirs

def main_loop_pic(ip: str) -> str:
    thegrid = parse(ip)
    s_loc, steps1, steps2, sdirs = main_loop(thegrid)
    in_the_loop = {s_loc} | {loc for loc, _, _ in steps1} | {loc for loc, _, _ in steps2}
    pic = thegrid.rect().picture(lambda loc: (thegrid[loc] if loc in in_the_loop else '.'))
    return pic

def distance_map(ip: str) -> str:
    thegrid = parse(ip)
    s_loc, steps1, steps2, sdirs = main_loop(thegrid)
    in_the_loop = {s_loc: 0} | {loc: count for loc, _, count in steps1} | {loc: count for loc, _, count in steps2}
    pic = thegrid.rect().picture(lambda loc: (str(in_the_loop[loc]) if loc in in_the_loop else '.'))
    return pic

def p1(ip: str) -> int:
    thegrid = parse(ip)
    s_loc, steps1, steps2, sdirs = main_loop(thegrid) 
    in_the_loop = {s_loc} | {loc for loc, _, _ in steps1} | {loc for loc, _, _ in steps2}
    pic = thegrid.rect().picture(lambda loc: (thegrid[loc] if loc in in_the_loop else '.'))
    print(pic)
    assert steps1[-1][2] == steps2[-1][2]
    return steps1[-1][2]

def get_outside_loop_set(ip: str) -> set[gint]:
    thegrid = parse(ip)
    s_loc, steps1, steps2, sdirs = main_loop(thegrid)

    for pstr, dirs in PIPES.items():
        if set(dirs) == set(sdirs):
            thegrid[s_loc] = pstr

    # print('BOOGA')
    # print(thegrid.rect().picture(lambda loc: thegrid[loc]))
    # print('tOOGA')

    loop = {s_loc} | {loc for loc, _, _ in steps1} | {loc for loc, _, _ in steps2}

    for z in thegrid.rect():
        if z not in loop:
            thegrid[z] = '.'

    print(thegrid.rect().picture(lambda z: thegrid[z]))
    input()

    def neighbours(point):
        # adjacent points, not including those where there's a boundary in between
        for direction in grid.NESW:
            adj = point + direction

            if adj not in thegrid.rect():
                continue

            if direction == grid.NORTH:
                spaces = point + grid.NW, point + grid.NORTH
                hv = True # horizontal
            elif direction == grid.EAST:
                spaces = point + grid.NORTH, point
                hv = False # vertical
            elif direction == grid.SOUTH:
                spaces = point + grid.WEST, point
                hv = True
            elif direction == grid.WEST:
                spaces = point + grid.NW, point + grid.WEST
                hv = False

            # if spaces[0] not in thegrid.rect() or spaces[1] not in thegrid.rect():
            #     continue

            space1, space2 = spaces
            # print('dir', direction, 'spaces', spaces, 'space1 in loop', space1 in loop, 'space2 in loop', space2 in loop)
            # print('spaces: ', thegrid[space1], thegrid[space2])

            if (
                space1 in loop and space2 in loop
                and pipes_are_opposite(thegrid[space1], thegrid[space2], hv)
            ):
                # print('direction', direction, 'GEBLOCKED at', point, 'spaces: ', spaces, 'pipes: ', thegrid[space1], thegrid[space2], 'hv:', hv)
                continue
            # else:
                # print('direction', direction, 'NOT GEBLOCKED at', point, 'spaces: ', spaces, 'pipes: ', thegrid[space1], thegrid[space2], 'hv:', hv)

            yield adj

    outside_loop = (
        {gint(0, j) for j in range(thegrid.height)} 
        | {gint(thegrid.width, j) for j in range(thegrid.height)}
        | {gint(i, 0) for i in range(thegrid.width)}
        | {gint(i, thegrid.height + 1) for i in range(thegrid.width)}
    )

    new_outside_loop = list(outside_loop)

    while True:
        # print()
        # # my code for p2() didn't work here, but by just copy-pasting the final form of this picture into my editor,
        # # and ctrl+f-ing for the string ' .', and counting the number of matches, i got the right answer
        # # (file d10_pic.txt contains the picture that was printed)
        # print(thegrid.rect().picture(lambda loc: (('^' if loc in loop else "'") + thegrid[loc] if loc in outside_loop else ('@' if loc in loop else ' ') + thegrid[loc])))
        # print()
        nbs = set(it.chain.from_iterable(map(neighbours, new_outside_loop)))
        new_outside_loop = nbs - outside_loop

        if not new_outside_loop:
            break

        outside_loop.update(new_outside_loop)

    return loop, outside_loop 

def enclosed_map(ip: str) -> str:
    thegrid = parse(ip)
    loop, outside_loop = get_outside_loop_set(ip)
    print(thegrid.rect().picture(lambda loc: 'O' if loc in outside_loop else 'I'))
    pic = thegrid.rect().picture(lambda loc: ('I' if loc not in outside_loop and thegrid[loc] == '.' else thegrid[loc]))
    print(pic)
    return pic

def p2(ip: str) -> int:
    thegrid = parse(ip)
    loop, outside_loop = get_outside_loop_set(ip)
    return sum(1 for z in thegrid.rect() if z not in outside_loop and z not in loop)


    # start with a point on the outside

    # we should be able to divide the grid into two regions, inside-the-loop
    # and outside-the-loop, using a flood fill strategy
    # we can identify outside by the fact that it includes points on the edge
    # of the grid
    # we should consider points 0.5 between the original points

    # O _ O _ O
    #  |_| |_|
    # O _ O _ O
    #  |_| |_|
    # O   O   O
    #  
    # the Os are points
    # there are four adjacent grid-spaces, one on each corner
    # between two Os that are adjacent horizontally/vertically, there are exactly 2 grid-spaces
    # we can pass between Os as long as the loop doesn't go *across* those grid-spaces
