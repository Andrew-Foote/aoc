from collections import defaultdict
from collections.abc import Iterator
import statistics
import time
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid

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
        ('visited_pos_pic', '''\
....#.....
....XXXXX#
....X...X.
..#.X...X.
..XXXXX#X.
..X.X.X.X.
.#XXXXXXX.
.XXXXXXX#.
#XXXXXXX..
......#X..'''),
        ('p1', 41),
        ('cycle_pos_pics', '''\
....#.....
....+---+#
....|...|.
..#.|...|.
....|..#|.
....|...|.
.#.O^---+.
........#.
#.........
......#...

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
......O.#.
#.........
......#...

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----+O#.
#+----+...
......#...

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
..|...|.#.
#O+---+...
......#...

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
....|.|.#.
#..O+-+...
......#...

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----++#.
#+----++..
......#O..'''),
        ('p2', 6)
    ]),
]

def parse(ip: str) -> tuple[Grid, gint]:
    grid_rows: list[list[str]] = []
    guard_pos: gint

    for y, line in enumerate(ip.splitlines()):
        grid_row: list[str] = []

        for x, char in enumerate(line):
            pos = gint(x, y)
            grid_row.append(char)

            if char == '^':
                guard_pos = pos

        grid_rows.append(grid_row)

    return Grid(grid_rows), guard_pos

def guard_path(grid: Grid, guard_pos: gint) -> Iterator[tuple[gint, gint]]:
    guard_dir = gint(0, -1)

    while True:
        yield guard_pos, guard_dir
        new_pos = guard_pos + guard_dir

        if new_pos not in grid:
            return

        char = grid[new_pos]

        if char == '#':
            guard_dir *= gint(0, 1)
        else:
            guard_pos += guard_dir

def visited_pos_pic(ip: str) -> str:
    grid, guard_pos = parse(ip)
    visited_set = {pos for pos, _ in guard_path(grid, guard_pos)}

    def pos_pic(pos: gint) -> str:
        if pos in visited_set:
            return 'X'
        else:
            return grid[pos]

    return grid.rect().picture(pos_pic)

def p1(ip: str) -> int:
    grid, guard_pos = parse(ip)
    return len({pos for pos, _ in guard_path(grid, guard_pos)})

def cyclic_guard_paths(
    grid: Grid, orig_guard_pos: gint
) -> Iterator[tuple[gint, dict[gint, set[gint]]]]:
    
    unobstructed_path = list(guard_path(grid, orig_guard_pos))
    unobstructed_seen = defaultdict(set)
    poss_cycle_pos_set: set[gint] = set()
    seen0: set[gint] = set()
    
    # maps each node (position/direction pair) in the unobstructed path to the
    # set of positions p such that, if the guard continues to travel along the
    # unobstructed path, they will end up facing p (so that their next step,
    # if unobstructed, would be to move to p)
    #
    # if p is the pos we've added the obstruction to, then as long as p does
    # not belong to this set, it should be the case that once the guard
    # reaches this node, they're going to leave the map
    facing_map: defaultdict[tuple[gint, gint], set[gint]] = defaultdict(set)

    for pos, dir in unobstructed_path:
        if grid[pos] == '.':
            poss_cycle_pos_set.add(pos)

        unobstructed_seen[pos].add(dir)

        seen0.add((pos, dir))

        for node in seen0:
            facing_map[node].add(pos + dir)

    for poss_cycle_pos in poss_cycle_pos_set:
        grid[poss_cycle_pos] = '#'
        seen: dict[gint, set[gint]] = defaultdict(set)
        gronk = None

        for guard_pos, guard_dir in guard_path(grid, orig_guard_pos):
            if guard_dir in seen[guard_pos]:
                if gronk is not None:
                    # print('SHOULD BE CYCLE, BUT LEAVING INSTEAD!!!')
                    # print('PIC IF UNOBSTRUCTED')
                    # print(cycle_pic(grid, guard_pos, gint(-1, -1), unobstructed_seen, None))
                    # print('LEAVING RESULT')
                    yield gronk
                    # print('CYCLE RESULT')
                    yield poss_cycle_pos, seen, None
                else:
                    # print('CYCLE!!!!')
                    yield poss_cycle_pos, seen, None
                    break
            else:
                seen[guard_pos].add(guard_dir)

            if gronk is None and (guard_pos, guard_dir) in unobstructed_seen and poss_cycle_pos not in facing_map[guard_pos, guard_dir]:
                # print('LEAVING!!!')
                gronk = poss_cycle_pos, seen, (guard_pos, guard_dir)

        grid[poss_cycle_pos] = '.'

        # if gronk is not None:
        #     yield gronk

def cycle_pic(
    grid: Grid, guard_pos: gint, cycle_pos: gint, seen: dict[gint, set[gint]],
    early_exit: tuple[gint, gint] | None=None
) -> str:
    
    def pos_pic(pos: gint) -> str:
        if early_exit is not None and pos == early_exit[0]:
            return {
                gint(0, -1): 'U', gint(0, 1): 'D',
                gint(-1, 0): 'L', gint(1, 0): 'R'
            }[early_exit[1]]

        if pos == guard_pos:
            return '^'

        if pos == cycle_pos:
            return 'O'

        dirs = seen[pos]
        has_vdir = gint(0, -1) in dirs or gint(0, 1) in dirs
        has_hdir = gint(-1, 0) in dirs or gint(1, 0) in dirs

        if has_vdir and has_hdir:
            return '+'
        elif has_vdir:
            return '|'
        elif has_hdir:
            return '-'
        else:
            return grid[pos]
        
    return grid.rect().picture(pos_pic)

def cycle_pos_pics(ip: str) -> str:
    grid, guard_pos = parse(ip)
    pics: dict[gint, str] = {}

    for cycle_pos, seen, early_exit in cyclic_guard_paths(grid, guard_pos):
        pics[cycle_pos] = cycle_pic(
            grid, guard_pos, cycle_pos, seen, early_exit
        )

    cycle_pos_list = sorted(pics.keys(), key=lambda pos: (pos.imag, pos.real))
    return '\n\n'.join(pics[pos] for pos in cycle_pos_list)

def p2(ip: str) -> int:
    grid, orig_guard_pos = parse(ip)
    
    return len({
        cycle_pos for cycle_pos, _, _
        in cyclic_guard_paths(grid, orig_guard_pos)
    })
