from collections.abc import Iterator

test_inputs = [('example', '''\
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.''', [
    ('accessible_rolls_pic', '''\
..xx.xx@x.
x@@.@.@.@@
@@@@@.x.@@
@.@@@@..@.
x@.@@@@.@x
.@@@@@@@.@
.@.@.@.@@@
x.@@@.@@@@
.@@@@@@@@.
x.x.@@@.x.'''),
    ('p1', 13)
])]

Point = tuple[int, int]



def adjacent_points(p: Point) -> tuple[Point]:
    x, y = p

    return (
        (x - 1, y - 1),
        (x - 1, y),
        (x - 1, y + 1),
        (x    , y - 1),
        (x    , y + 1),
        (x + 1, y - 1),
        (x + 1, y),
        (x + 1, y + 1)
    )

def parse(ip: str) -> set[Point]:
    result: set[Point] = set()

    for y, line in enumerate(ip.splitlines()):
        for x, c in enumerate(line):
            if c == '@':
                result.add((x, y))

    return result

def accessible_rolls(ip: str) -> Iterator[Point]:
    grid = parse(ip)

    for p in grid:
        adjacent_roll_count = sum(
            1 for q in adjacent_points(p) if q in grid
        )

        accessible = adjacent_roll_count < 4
        
        if accessible:
            yield p

def accessible_rolls_pic(ip: str) -> str:
    return ''

def p1(ip: str) -> int:
    return sum(1 for _ in accessible_rolls(ip))