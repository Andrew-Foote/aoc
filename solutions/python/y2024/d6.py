from collections import defaultdict
from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from solutions.python.lib.grid2 import Grid, Point, Rect, Vec
import solutions.python.lib.grid2 as g

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

Node = tuple[Point, Vec]

def parse(ip: str) -> tuple[Grid, Node]:
    grid_rows: list[list[str]] = []
    guard_pos: Point

    for y, line in enumerate(ip.splitlines()):
        grid_row: list[str] = []

        for x, char in enumerate(line):
            pos = Point(x, y)
            grid_row.append(char)

            if char == '^':
                guard_pos = pos

        grid_rows.append(grid_row)

    return Grid(grid_rows), (guard_pos, g.NORTH)

@dataclass(frozen=True, slots=True)
class ModifiedGrid:
    orig_grid: Grid
    new_obstacle: Point

    def rect(self) -> Rect:
        return self.orig_grid.rect()

    def __getitem__(self, p: Point) -> str:
        if p == self.new_obstacle:
            return '#'
        
        return self.orig_grid[p]
    
GridLike = Grid | ModifiedGrid

def get_next_node(grid: GridLike, node: Node) -> Node | None:
    p, d = node

    while True:
        q = p + d

        if q not in grid.rect():
            return None
        
        if grid[q] == '#':
            return p, d.rot_clockwise()
        
        p = q

def get_path(grid: GridLike, start_node: Node) -> Iterator[Node]:
    node: Node | None = start_node

    while node is not None:
        yield node
        node = get_next_node(grid, node)

def get_full_path(grid: GridLike, path: Iterable[Node | None]) -> list[Node]:
    result: list[Node] = []
    prev_node: Node | None = None

    for cur_node in path:
        if cur_node is None:
            assert prev_node is not None
            prev_pos, prev_dir = prev_node
            intermediate_pos = prev_pos

            while True:
                intermediate_pos += prev_dir

                if intermediate_pos not in grid.rect():
                    break
                else:
                    result.append((intermediate_pos, prev_dir))
        else:
            cur_pos, cur_dir = cur_node

            if prev_node is not None:
                prev_pos, prev_dir = prev_node
                intermediate_pos = prev_pos

                while True:
                    intermediate_pos += prev_dir
                    result.append((intermediate_pos, prev_dir))

                    if intermediate_pos == cur_pos:
                        break

            result.append((cur_pos, cur_dir))

        prev_node = cur_node

    return result

def get_visited_set(grid: Grid, start_node: Node) -> set[Point]:
    short_path = list(get_path(grid, start_node)) + [None]
    return {p for p, _ in get_full_path(grid, short_path)}

def visited_pos_pic(ip: str) -> str:
    grid, start_node = parse(ip)
    visited = get_visited_set(grid, start_node)
    return grid.rect().picture(lambda p: 'X' if p in visited else grid[p])

def p1(ip: str) -> int:
    grid, start_node = parse(ip)
    visited = get_visited_set(grid, start_node)
    return len(visited)

def get_cycle_positions(
    grid: Grid, start_node: Node
) -> Iterator[tuple[Point, list[Node]]]:

    orig_visited = get_visited_set(grid, start_node)

    for poss_cycle_pos in orig_visited:
        new_grid = ModifiedGrid(grid, poss_cycle_pos)
        path: list[Node] = []
        seen: set[Node] = set()

        for p, d in get_path(new_grid, start_node):
            if (p, d) in seen:
                path.append((p, d))
                yield poss_cycle_pos, path
                break
        
            path.append((p, d))
            seen.add((p, d))

CycleInfo = dict[Point, set[Vec]]

def get_cycle_pic(
    grid: Grid, start_pos: Point, cycle_pos: Point, seen: CycleInfo
) -> str:
    
    def draw(p: Point) -> str:
        if p == start_pos:
            return '^'

        if p == cycle_pos:
            return 'O'

        dirs = seen[p]
        has_vdir = g.NORTH in dirs or g.SOUTH in dirs
        has_hdir = g.EAST in dirs or g.WEST in dirs

        if has_vdir and has_hdir:
            return '+'
        elif has_vdir:
            return '|'
        elif has_hdir:
            return '-'
        else:
            return grid[p]
        
    return grid.rect().picture(draw)

def cycle_pos_pics(ip: str) -> str:
    grid, start_node = parse(ip)
    pics: dict[Point, str] = {}

    for cycle_pos, short_path in get_cycle_positions(grid, start_node):
        path = get_full_path(grid, short_path)
        seen: CycleInfo = defaultdict(set)

        for p, d in path:
            seen[p].add(d)


        pics[cycle_pos] = get_cycle_pic(grid, start_node[0], cycle_pos, seen)

    cycle_pos_list = sorted(pics.keys(), key=lambda pos: (pos.y, pos.x))
    return '\n\n'.join(pics[pos] for pos in cycle_pos_list)

def p2(ip: str) -> int:
    grid, start_node = parse(ip)
    import time
    t0 = time.perf_counter_ns()

    r = len({
        cycle_pos for cycle_pos, _ in get_cycle_positions(grid, start_node)
    })

    t1 = time.perf_counter_ns()
    print(f'time elapsed: {(t1 - t0) / 1_000_000_000} seconds')
    return r