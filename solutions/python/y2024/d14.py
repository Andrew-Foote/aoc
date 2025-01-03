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
    ]),
    # https://old.reddit.com/r/adventofcode/comments/1he00zu/year_2024_day_14_part_2_is_that_a_tree_in_this/
    ('reddit-example', '''\
p=20,34 v=-1,5
p=21,4 v=4,9
p=22,18 v=-8,14
p=23,3 v=2,16
p=24,48 v=12,10
p=25,35 v=16,-2
p=26,51 v=15,-11
p=27,5 v=2,2
p=28,18 v=16,14
p=29,8 v=-20,-19
p=30,47 v=12,17
p=31,8 v=-8,-19
p=32,19 v=-7,7
p=33,65 v=-19,-6
p=34,78 v=2,6
p=35,66 v=-16,-13
p=36,80 v=0,-8
p=37,19 v=5,7
p=38,66 v=11,-13
p=39,79 v=-9,-1
p=40,92 v=1,11
p=41,65 v=10,-6
p=42,78 v=-1,6
p=43,93 v=-5,4
p=44,51 v=-4,-11
p=20,49 v=-3,10
p=44,78 v=6,13
p=20,22 v=-10,0
p=44,83 v=-5,-15
p=20,84 v=-10,-15
p=27,35 v=10,19
p=28,39 v=15,-9
p=38,24 v=8,-7
p=39,8 v=12,2
p=40,39 v=1,-9
p=44,80 v=-15,13
p=20,9 v=19,2
p=26,56 v=-1,-18
p=27,95 v=13,18
p=28,25 v=-4,-7
p=37,83 v=-18,-1
p=41,81 v=18,13
p=44,53 v=7,3
p=20,42 v=-9,-16
p=25,71 v=-9,-13
p=26,101 v=10,-17
p=27,96 v=3,18
p=28,9 v=-17,9
p=41,38 v=-9,12
p=44,11 v=1,-5
p=20,28 v=-4,-14
p=24,57 v=18,-11
p=25,99 v=-15,4
p=27,102 v=12,-17
p=28,85 v=-7,-1
p=29,72 v=14,-13
p=41,25 v=14,7
p=44,42 v=-10,-9
p=20,100 v=-10,4
p=24,74 v=16,-20
p=25,85 v=12,6
p=26,74 v=3,-20
p=27,57 v=13,-4
p=28,26 v=-14,7
p=29,59 v=-5,-18
p=40,101 v=2,-3
p=44,26 v=-12,7
p=20,71 v=-7,8
p=25,16 v=17,-19
p=26,42 v=-13,5
p=27,14 v=10,-5
p=28,59 v=1,-11
p=29,16 v=3,-19
p=30,44 v=-4,-9
p=33,43 v=17,-2
p=34,29 v=16,-7
p=35,70 v=-2,15
p=36,55 v=-11,17
p=40,58 v=12,-4
p=44,85 v=-5,13
p=20,72 v=-7,8
p=26,17 v=-1,-19
p=27,56 v=-1,17
p=28,28 v=-10,7
p=29,27 v=-1,14
p=30,17 v=12,-19
p=31,42 v=-11,12
p=32,56 v=-12,17
p=33,73 v=-12,1
p=34,72 v=16,8
p=35,100 v=-14,18
p=36,43 v=-1,5
p=37,46 v=-10,-16
p=38,44 v=-18,-2
p=39,43 v=-8,5
p=44,56 v=12,17
p=20,73 v=19,8
p=26,42 v=16,19
p=27,15 v=-14,2
p=28,1 v=5,-3
p=29,89 v=5,-1
p=30,17 v=16,-12
p=31,57 v=0,17
p=32,90 v=12,-8
p=33,46 v=-10,-9
p=34,43 v=-7,12
p=35,30 v=-1,0
p=36,44 v=-5,5
p=37,76 v=19,-13
p=38,31 v=-5,-7
p=44,72 v=11,15
p=20,89 v=-3,6
p=26,61 v=18,-4
p=27,15 v=4,9
p=28,90 v=19,-1
p=29,88 v=-20,13
p=30,19 v=-14,-19
p=31,92 v=-12,-15
p=32,59 v=-2,10
p=33,75 v=-16,1
p=34,48 v=1,-16
p=35,19 v=6,-19
p=36,33 v=12,-14
p=37,74 v=-5,8
p=44,47 v=-19,-9
p=20,62 v=-6,-4
p=27,60 v=-18,10
p=28,15 v=5,16
p=29,48 v=-3,-9
p=30,4 v=1,-10
p=31,3 v=-12,-3
p=32,32 v=8,0
p=33,33 v=2,-7
p=34,48 v=12,-9
p=35,18 v=9,-5
p=36,59 v=6,17
p=37,89 v=11,13
p=44,64 v=-18,-18
p=20,50 v=-13,-16
p=27,80 v=-9,-20
p=28,33 v=3,0
p=29,35 v=-15,-14
p=30,48 v=14,-2
p=31,79 v=16,-13
p=32,21 v=-8,-19
p=33,47 v=-2,5
p=34,62 v=2,3
p=35,62 v=11,3
p=36,64 v=15,-11
p=37,90 v=-4,13
p=44,32 v=-15,7
p=20,33 v=18,7
p=26,91 v=9,13
p=27,94 v=-17,-8
p=28,92 v=7,6
p=29,64 v=3,-4
p=30,76 v=3,15
p=31,36 v=-3,-14
p=34,17 v=0,16
p=35,50 v=16,-9
p=36,33 v=6,7
p=44,4 v=18,4
p=20,3 v=3,18
p=25,8 v=-3,-17
p=26,6 v=15,-3
p=27,96 v=-17,-15
p=30,65 v=0,-4
p=31,5 v=0,4
p=33,8 v=-10,-17
p=34,66 v=7,-11
p=35,62 v=2,17
p=36,80 v=5,-6
p=37,23 v=-5,-19
p=44,20 v=16,2
p=20,52 v=-5,-9
p=25,79 v=-13,8
p=26,20 v=0,9
p=30,65 v=-20,3
p=31,83 v=-11,-20
p=33,66 v=-5,-4
p=34,4 v=8,18
p=36,34 v=-19,14
p=37,53 v=-8,-16
p=44,24 v=-1,-19
p=20,82 v=-7,-6
p=44,5 v=13,18
p=20,97 v=12,-1
p=44,6 v=-20,18
p=20,22 v=-7,16
p=21,41 v=3,-14
p=22,25 v=-18,-5
p=23,51 v=-7,19
p=24,23 v=-1,9
p=25,100 v=-3,-15
p=26,11 v=-9,-10
p=27,83 v=11,1
p=28,9 v=-8,4
p=29,26 v=-5,-12
p=30,81 v=11,15
p=31,27 v=12,-19
p=32,9 v=1,4
p=33,41 v=1,-14
p=34,81 v=-14,15
p=35,68 v=10,3
p=36,9 v=-20,4
p=37,70 v=4,-11
p=38,70 v=-17,-11
p=39,11 v=15,-10
p=40,83 v=11,1
p=41,71 v=-8,-18
p=42,69 v=4,-4
p=43,97 v=7,6
p=44,54 v=9,-2
p=2,3 v=-7,-4
p=7,12 v=12,-18
p=34,102 v=-1,-12
p=89,36 v=-20,-15
p=6,61 v=-19,16
p=6,22 v=-1,-8
p=94,18 v=-13,-8
p=61,98 v=17,19
p=59,50 v=-17,-20
p=91,22 v=-13,16
p=48,28 v=-17,-6
p=94,33 v=18,-16
p=18,40 v=-5,1
p=59,9 v=-5,8
p=14,17 v=-6,-7
p=90,88 v=17,-14
p=94,47 v=-10,-7
p=38,91 v=-16,0
p=65,45 v=10,-6
p=88,77 v=-3,4
p=83,79 v=-14,15
p=42,5 v=-9,17
p=56,31 v=8,13
p=95,3 v=-15,-19
p=97,74 v=8,19
p=84,19 v=8,-8
p=53,9 v=3,0
p=4,1 v=4,5
p=13,64 v=-7,-5
p=100,16 v=5,15
p=72,5 v=3,-10
p=5,62 v=12,-5
p=59,51 v=-10,-8
p=62,98 v=12,9
p=69,74 v=14,-8
p=72,39 v=-12,-5
p=69,94 v=19,4
p=5,94 v=15,-2
p=17,66 v=11,-13
p=25,73 v=-8,-8
p=75,37 v=-18,13
p=1,33 v=10,13
p=0,6 v=-9,-15
p=70,31 v=-8,1
p=51,8 v=-1,12
p=79,90 v=-13,17
p=58,82 v=8,11
p=100,7 v=5,7
p=43,14 v=-14,-13
p=10,50 v=3,3
p=7,79 v=-3,-5
p=90,61 v=-9,-10
p=46,16 v=1,-2
p=98,100 v=-14,-12
p=82,58 v=14,-14
p=72,5 v=9,-13
p=77,91 v=-16,14
p=55,61 v=14,-12
p=8,86 v=-1,13
p=1,11 v=-5,17
p=3,26 v=18,12
p=98,101 v=4,-2
p=27,22 v=-7,8
p=16,100 v=-13,-14
p=12,61 v=-8,9
p=76,61 v=14,14
p=1,82 v=10,-12
p=6,17 v=-5,17
p=74,40 v=11,-11
p=2,6 v=-17,9
p=92,22 v=15,4
p=43,8 v=11,0
p=97,70 v=2,17
p=64,71 v=-18,-12
p=12,84 v=12,10
p=69,59 v=2,11
p=76,3 v=-20,-11
p=14,75 v=12,-5
p=6,1 v=-3,-3
p=77,95 v=-4,-14
p=99,27 v=-7,7
p=16,9 v=-16,9
p=43,78 v=-2,-19
p=10,85 v=-3,3
p=5,0 v=19,-4
p=96,20 v=-15,-3
p=55,45 v=5,6
p=14,66 v=-2,14
p=88,20 v=19,10
p=27,16 v=-18,-4
p=98,60 v=-16,-2
p=12,20 v=5,-3
p=88,24 v=2,2
p=66,84 v=0,4
p=49,78 v=-17,2
p=45,102 v=16,-14
p=35,29 v=14,13
p=51,68 v=-13,-1
p=58,43 v=18,11
p=2,42 v=18,0
p=81,65 v=19,17
p=18,24 v=2,12
p=49,59 v=9,-19
p=21,52 v=14,9
p=65,32 v=-18,-8
p=70,57 v=-20,10
p=71,93 v=-4,-20
p=59,42 v=-5,1
p=3,84 v=-1,-3
p=99,64 v=0,11
p=9,65 v=-4,-16
p=100,36 v=-2,-8
p=0,39 v=-13,14
p=5,92 v=6,-2
p=69,87 v=-19,2
p=100,38 v=-8,-12
p=18,97 v=7,3
p=61,90 v=13,12
p=99,95 v=-20,15
p=64,13 v=1,-14
p=52,25 v=-2,8
p=50,85 v=-13,-7
p=34,5 v=-6,12
p=100,46 v=11,2
p=12,88 v=4,10
''', [
    ('p2', 2222),
])]

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
    
def display_robots(
    robots: list[Robot], width: int, height: int, i: int
)-> None:

    cur_robots = [robot.step(width, height, i) for robot in robots]
    occupied_points = {robot.pos for robot in cur_robots}
    rect = Rect.from_tlwh(0, 0, width, height)

    print('=' * 80)
    print(f' STEP {i}')
    print('=' * 80)
    print()
    print(rect.picture(lambda p: '🮋' if p in occupied_points else '.'))
    print()

def p2(ip: str) -> int:
    width, height, robots = parse(ip)
    rect = Rect.from_tlwh(0, 0, width, height)

    print(
        "\n\nYou will be shown a series of pictures of the robots' positions.\n"
        "For each picture you will be asked to input 'y' or 'n'.\nTo begin"
        " with, answer 'n' every time, while looking for a pattern that"
        " reoccurs at regular intervals.\nOnce you think you've found such a"
        " pattern, answer 'y' on two consecutive instances of the pattern.\n"
    )

    ys: list[int] = []

    for i in it.count():
        display_robots(robots, width, height, i)

        ans = ''

        while ans not in ('y', 'n'):
            ans = input("Input 'y' or 'n' (default is 'n'). ") or 'n'

            if ans == 'y':
                ys.append(i)

        if len(ys) >= 2:
            break

    assert len(ys) == 2
    interval = ys[1] - ys[0]
    start = ys[0] % interval
    
    print(
        f"\nOK, so the pattern occurs at step {ys[0]} and every {interval}"
        " steps afterwards. Now, input 'y' when you see the robots arrange"
        " themselves as an image."
    )

    for i0 in it.count():
        i = start + i0 * interval
        display_robots(robots, width, height, i)

        ans = ''

        while ans not in ('y', 'n'):
            ans = input("Input 'y' or 'n' (default is 'n'). ") or 'n'

            if ans == 'y':
                return i
            
    assert False