from collections.abc import Generator
from collections import defaultdict
from dataclasses import dataclass
from enum import Enum
import functools as ft
import itertools as it
import math
from typing import assert_never, Self
from solutions.python.lib.gint import gint

test_inputs = [
    ('example', '2333133121414131402', [
        ('blocks_s', '00...111...2...333.44.5555.6666.777.888899'),
        ('moves_s', '''\
00...111...2...333.44.5555.6666.777.888899
009..111...2...333.44.5555.6666.777.88889.
0099.111...2...333.44.5555.6666.777.8888..
00998111...2...333.44.5555.6666.777.888...
009981118..2...333.44.5555.6666.777.88....
0099811188.2...333.44.5555.6666.777.8.....
009981118882...333.44.5555.6666.777.......
0099811188827..333.44.5555.6666.77........
00998111888277.333.44.5555.6666.7.........
009981118882777333.44.5555.6666...........
009981118882777333644.5555.666............
00998111888277733364465555.66.............
0099811188827773336446555566..............
'''),
        ('p1', 1928),
    ]),
    ('example2', '12345', [
        ('blocks_s', '0..111....22222'),   
        ('moves_s', '''\
0..111....22222
02.111....2222.
022111....222..
0221112...22...
02211122..2....
022111222......'''),     
    ]),
]

def parse(ip: str) -> list[int]:
    return [int(d) for d in ip]

def blocks_s(ip: str) -> str:
    result: list[str] = []
    cur_file_id: int = 0

    for i, d in enumerate(parse(ip)):
        q, r = divmod(i, 2)

        if r: # free block
            result.append('.' * d)
        else: # free block
            file_id = str(q)
            result.append(file_id * d)

    return ''.join(result)