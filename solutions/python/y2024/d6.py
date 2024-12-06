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
    ]),
]

# ^ - guyard pos (facing up)
# # - obstacle
# guard ai:
# - if something is in frotn turn right 90 deg
# - otherwise step fwd

# how many distinct positions will the guard visit
# before leaving mapped area

def p1(ip: str) -> int:
    grid: dict[gint, str] = {}
    guard_pos: gint
    guard_dir: gint = gint(0, -1)

    for y, line in enumerate(ip.splitlines()):
        for x, char in enumerate(line):
            pos = gint(x, y)
            grid[pos] = char

            if char == '^':
                guard_pos = pos

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