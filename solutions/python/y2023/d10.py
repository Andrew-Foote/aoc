from solutions.python.lib.gint import gint
import solutions.python.lib.grid as grid

test_inputs = [
    ('example1', '''\
.....
.S-7.
.|.|.
.L-J.
.....''', [
        ('distance_map', '''\
.....
.012.
.1.3.
.234.
.....'''),
        ('p1', 4)
    ]),
    ('example2', '''\
-L|F7
7S-7|
L|7||
-L-J|
L|-JF''', [
        ('main_loop_pic', '''\
.....
.S-7.
.|.|.
.L-J.
.....''')
    ]),
    ('example3', '''\
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ''', [
        ('main_loop_pic', '''\
..F7.
.FJ|.
SJ.L7
|F--J
LJ...'''),
        ('distance_map', '''\
..45.
.236.
01.78
14567
23...'''),
        ('p1', 8)
    ])
]

PIPES = {
    '|': (grid.NORTH, grid.SOUTH),
    '-': (grid.EAST, grid.WEST),
    'L': (grid.NORTH, grid.EAST),
    'J': (grid.NORTH, grid.WEST),
    '7': (grid.SOUTH, grid.WEST),
    'F': (grid.SOUTH, grid.EAST),
}

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

def main_loop(thegrid: grid.Grid[str]) -> tuple[gint, list[tuple[gint, gint, int]], list[tuple[gint, gint, int]]]:
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

    # follow the path from each connector till they meet; this will be the point
    # of furthest distance

    cur1, cur2 = connectors

    while True:
        print('cur is', cur1)
        cur1_loc, cur1_dir, cur1_steps = cur1
        cur_pipe = PIPES[thegrid[cur1_loc]]
        print(cur_pipe, cur_pipe[0], cur_pipe[1])
        next_dir = [next_dir for next_dir in cur_pipe if next_dir != -cur1_dir][0]
        print('nextidr is', next_dir)
        next_loc = cur1_loc + next_dir
        print(next_loc, thegrid[next_loc], repr(next_dir), repr(-next_dir), PIPES[thegrid[next_loc]], -next_dir in PIPES[thegrid[next_loc]])
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

    return s_loc, steps1, steps2

def main_loop_pic(ip: str) -> str:
    thegrid = parse(ip)
    s_loc, steps1, steps2 = main_loop(thegrid)
    in_the_loop = {s_loc} | {loc for loc, _, _ in steps1} | {loc for loc, _, _ in steps2}
    pic = thegrid.rect().picture(lambda loc: (thegrid[loc] if loc in in_the_loop else '.'))
    print(pic)
    return pic

def distance_map(ip: str) -> str:
    thegrid = parse(ip)
    s_loc, steps1, steps2 = main_loop(thegrid)
    in_the_loop = {s_loc: 0} | {loc: count for loc, _, count in steps1} | {loc: count for loc, _, count in steps2}
    pic = thegrid.rect().picture(lambda loc: (str(in_the_loop[loc]) if loc in in_the_loop else '.'))
    print(pic)
    return pic

def p1(ip: str) -> grid.Grid[str]:
    thegrid = parse(ip)
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

        cur2_loc, cur2_dir, cur2_steps = cur2
        cur_pipe = PIPES[thegrid[cur2_loc]]
        next_dir = [next_dir for next_dir in cur_pipe if next_dir != -cur2_dir][0]
        next_loc = cur2_loc + next_dir
        assert thegrid[next_loc] in PIPES and -next_dir in PIPES[thegrid[next_loc]]
        cur2 = next_loc, next_dir, cur2_steps + 1

        if cur1[0] == cur2[0]:
            assert cur1[2] == cur2[2]
            return cur1[2]