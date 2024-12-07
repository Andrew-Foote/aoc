from collections import defaultdict
from collections.abc import Iterator
import functools as ft
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid
from solutions.python.lib import grid as g

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

Node = tuple[gint, gint] # (position, direction)

def get_next_node(grid: Grid, node: Node) -> Node | None:
    pos, dir = node
    next_pos = pos + dir

    if next_pos not in grid:
        return None
    
    char = grid[next_pos]

    if char == '#':
        return pos, dir * g.SOUTH
    else:
        return next_pos, dir

def get_path(grid: Grid, start: Node) -> Iterator[Node]:
    node: Node | None = start

    while node is not None:
        # print(node)
        yield node
        node = get_next_node(grid, node)

def guard_path(grid: Grid, start: gint) -> Iterator[Node]:
    yield from get_path(grid, (start, g.NORTH))

def visited_pos_pic(ip: str) -> str:
    grid, start = parse(ip)
    visited = {pos for pos, _ in guard_path(grid, start)}
    return grid.rect().picture(lambda pos: 'X' if pos in visited else grid[pos])

def p1(ip: str) -> int:
    grid, start = parse(ip)
    return len({pos for pos, _ in guard_path(grid, start)})

SUCCESSORS: defaultdict[tuple[Grid, Node], set[gint]] = defaultdict(set)

def get_successors(grid: Grid, start: Node) -> set:
    global SUCCESSORS
    print(f'get_successors {start}', end=' ')
    result: defaultdict[Node, set[gint]] = defaultdict(set)
    seen: set[Node] = set()

    for node in get_path(grid, start):
        if (grid, node) in SUCCESSORS:
            cached_successors = SUCCESSORS[grid, node]
            print('CACHE HIT!', cached_successors)

            for seen_node in seen:
                result[seen_node].update(cached_successors)

            break
        else:
            print('CACHE MISS!')

            if node in seen:
                print(f'  {node}  IN SEEN')
                break

            print(f' {node} NOT IN SEEN')
            seen.add(node)

            for seen_node in seen:
                result[seen_node].add(node[0])

    resultdict = dict(result)

    for node, succs in result.items():
        SUCCESSORS[grid, node] |= succs

    return result[start]

def cyclic_guard_paths(
    grid: Grid, orig_guard_pos: gint
) -> Iterator[tuple[gint, dict[gint, set[gint]], tuple[gint, gint] | None]]:

    poss_cycle_pos_set = {pos for pos, _ in guard_path(grid, orig_guard_pos)}

    for poss_cycle_pos in poss_cycle_pos_set:
        modified_grid = grid.copy()
        modified_grid[poss_cycle_pos] = '#'

        seen: dict[gint, set[gint]] = defaultdict(set)
        exit_info: tuple[gint, dict[gint, set[gint]], tuple[gint, gint]] | None = None

        for guard_pos, guard_dir in guard_path(modified_grid, orig_guard_pos):
            if guard_dir in seen[guard_pos]:
                yield poss_cycle_pos, seen, None
                break
            else:
                seen[guard_pos].add(guard_dir)

            if (
                exit_info is None
                # yeah these two conditions are necessary
                # unless.. we make facing_map cover ALL nodes
                # and guard_pos in unobstructed_seen
                # and guard_dir in unobstructed_seen[guard_pos]
                and poss_cycle_pos not in get_successors(grid, (guard_pos, guard_dir))
            ):
                exit_info = poss_cycle_pos, seen, (guard_pos, guard_dir)
                break

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
    