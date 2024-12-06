from collections.abc import Iterator
import statistics
import time
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

def p1(ip: str) -> int:
    grid, guard_pos = parse(ip)
    return len({pos for pos, _ in guard_path(grid, guard_pos)})

def p2(ip: str) -> int:
    grid, orig_guard_pos = parse(ip)

    open_pos_set = {
        pos for pos, _ in guard_path(grid, orig_guard_pos)
        if grid[pos] == '.'
    }

    print(len(open_pos_set))
    input()

    cycle_pos_set: set[gint] = set()

    times: list[int] = []

    for open_pos in open_pos_set:
        start_time = time.perf_counter_ns()
        grid[open_pos] = '#'
        seen: set[tuple[gint, gint]] = set()

        for guard_pos, guard_dir in guard_path(grid, orig_guard_pos):
            if (guard_pos, guard_dir) in seen:
                cycle_pos_set.add(open_pos)
                break
            else:
                seen.add((guard_pos, guard_dir))

        grid[open_pos] = '.'
        end_time = time.perf_counter_ns()
        time_diff = end_time - start_time
        times.append(time_diff)
        print(f'time for {open_pos} iteration was {time_diff} ns')
        print(f'running avg so far: {statistics.mean(times)}')
        # avg for p2 is 13123695.015102698
        # 

    return len(cycle_pos_set)