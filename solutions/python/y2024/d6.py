from solutions.python.lib.gint import gint

test_inputs = [
    ('example', '''\
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...''', [
        ('p1', 41),
        ('p2', 6)
    ]),
]

# ^ - guyard pos (facing up)
# # - obstacle
# guard ai:
# - if something is in frotn turn right 90 deg
# - otherwise step fwd

# how many distinct positions will the guard visit
# before leaving mapped area

Grid = dict[gint, str]

def parse(ip: str) -> tuple[Grid, gint]:
    grid: dict[gint, str] = {}
    guard_pos: gint

    for y, line in enumerate(ip.splitlines()):
        for x, char in enumerate(line):
            pos = gint(x, y)
            grid[pos] = char

            if char == '^':
                guard_pos = pos

    return grid, guard_pos

def p1(ip: str) -> int:
    grid, guard_pos = parse(ip)
    guard_dir = gint(0, -1)
    path: set[gint] = {guard_pos}

    while True:
        new_pos = guard_pos + guard_dir

        if new_pos not in grid:
            return len(path)
        
        char = grid[new_pos]

        if char == '#':
            guard_dir *= gint(0, 1)
        else:
            guard_pos += guard_dir
            path.add(guard_pos)

    assert False

def p2(ip: str) -> int:
    grid: dict[gint, str] = {}
    orig_guard_pos: gint
    open_pos_set: set[gint] = set()

    for y, line in enumerate(ip.splitlines()):
        for x, char in enumerate(line):
            pos = gint(x, y)
            grid[pos] = char

            if char == '^':
                orig_guard_pos = pos
            elif char == '.':
                open_pos_set.add(pos)

    cycle_pos_set = set()

    for open_pos in open_pos_set:
        grid[open_pos] = '#'

        guard_pos = orig_guard_pos
        guard_dir = gint(0, -1)
        path: set[tuple[gint, gint]] = set()

        while True:
            if (guard_pos, guard_dir) in path:
                cycle_pos_set.add(open_pos)
                break
            else:
                path.add((guard_pos, guard_dir))

            new_pos = guard_pos + guard_dir

            if new_pos not in grid:
                break
           
            char = grid[new_pos]

            if char == '#':
                guard_dir *= gint(0, 1)
            else:
                guard_pos += guard_dir

        grid[open_pos] = '.'

    return len(cycle_pos_set)