from collections.abc import Generator
import re
from solutions.python.lib.gint import gint

test_inputs = [
    ('example', '''
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3''', [
        ('p1', 12),
    ])
]

def parse(ip: str) -> Generator[tuple[gint, gint]]:
    for line in ip.splitlines():
        if not line: continue
        m = re.match(r'p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)', line.strip())
        assert m is not None, line
        px, py, vx, vy = map(int, m.groups())
        yield (gint(px, py), gint(vx, vy))

# space is 101 width, 103 height (but example is 11 width, 7 heught)
# robots wrap around when going off edges

# safety factor is, after their movements, count of robots in each
# quadrant. since width and height are odd, middle row/column don't
# count as in any quadrant

def quadrant(w: int, h: int, pos: gint) -> int | None:
    x, y = pos.rect()

    if x < w // 2:
        if y < h // 2:
            # top left
            return 0
        elif y == h // 2:
            # left middle
            return None
        else:
            # bottom left
            return 2
    elif x == w // 2:
        return None
    elif y < h // 2:
        # top right
        return 1
    elif y == h // 2:
        return None
    else:
        # bottom right
        return 3

def p1(ip: str) -> int:
    # W = 11
    # H = 7
    W = 101
    H = 103

    quadrant_counts = [0, 0, 0, 0]

    for pos, vel in parse(ip):
        end_pos = pos + 100 * vel
        ex = end_pos.real % W
        ey = end_pos.imag % H
        clip_pos = gint(ex, ey)
        q = quadrant(W, H, clip_pos)

        if q is not None:
            quadrant_counts[quadrant(W, H, clip_pos)] += 1

    print(quadrant_counts)

    return (
        quadrant_counts[0] * quadrant_counts[1]
        * quadrant_counts[2] * quadrant_counts[3]
    )