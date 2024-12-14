from dataclasses import dataclass, replace
import itertools as it
import math
import re
from typing import Self
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Rect

test_inputs = [
    ('example', '''\
#w=11,h=7
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

@dataclass(frozen=True)
class Robot:
    pos: gint
    vel: gint

    def step(self, width: int, height: int, n: int) -> Self:
        new_pos = self.pos + n * self.vel
        clipped_new_pos = gint(new_pos.real % width, new_pos.imag % height)
        return replace(self, pos=clipped_new_pos)

    def quadrant(self, width: int, height: int) -> int | None:
        x, y = self.pos.rect()
        wmid = width // 2
        hmid = height // 2

        if x < wmid and y < hmid:
            return 0 # top left
        elif x > wmid and y < hmid:
            return 1 # top right
        elif x < wmid and y > hmid:
            return 2 # bottom left
        elif x > wmid and y > hmid:
            return 3 # bottom right

        return None

def parse(ip: str) -> tuple[int, int, list[Robot]]:
    robots: list[Robot] = []
    lines = ip.splitlines()
    assert lines
    m = re.match(r'#w=(\d+),h=(\d+)', lines[0])

    if m is not None:
        width, height = map(int, m.groups())
        lines = lines[1:]
    else:
        width, height = 101, 103

    for line in lines:
        if not line: continue
        m = re.match(r'p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)', line.strip())
        assert m is not None, line
        px, py, vx, vy = map(int, m.groups())
        robots.append(Robot(gint(px, py), gint(vx, vy)))

    return width, height, robots

def p1(ip: str) -> int:
    width, height, robots = parse(ip)
    quadrant_counts = [0, 0, 0, 0]

    for robot in robots:
        moved_robot = robot.step(width, height, 100)
        q = moved_robot.quadrant(width, height)

        if q is not None:
            quadrant_counts[q] += 1

    return math.prod(quadrant_counts)

def p2(ip: str) -> None:
    width, height, robots = parse(ip)
    rect = Rect.from_tlwh(0, 0, width, height)

    for i in it.count():
        occupied_points = {robot.pos for robot in robots}

        if i % 101 == 70:
            print('=' * 80)
            print(f' STEP {i}')
            print('=' * 80)
            print()
            print(rect.picture(lambda p: '🮋' if p in occupied_points else '.'))
            input()

        for j, robot in enumerate(robots):
            robots[j] = robot.step(width, height, 1)