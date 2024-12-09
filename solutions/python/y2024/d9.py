from collections.abc import Generator
from collections import defaultdict, deque
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
        ('states_s', '''\
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
0099811188827773336446555566..............'''),
        ('p1', 1928),
    ]),
    ('example2', '12345', [
        ('blocks_s', '0..111....22222'),   
        ('states_s', '''\
0..111....22222
02.111....2222.
022111....222..
0221112...22...
02211122..2....
022111222......'''),     
    ]),
]

def parse(ip: str) -> list[int]:
    return [int(d) for d in ip.strip()]

@dataclass
class FreeBlock:
    def __str__(self) -> str:
        return '.'

@dataclass
class FileBlock:
    file_id: int

    def __str__(self) -> str:
        return str(self.file_id)

Block = FreeBlock | FileBlock

def iterblocks(ip: str) -> Generator[Block]:
    for i, d in enumerate(parse(ip)):
        q, r = divmod(i, 2)

        if r: # free block
            for _ in range(d):
                yield FreeBlock()
        else: # free block
            for _ in range(d):
                yield FileBlock(q)

def blocks_s(ip: str) -> str:
    return ''.join(str(block) for block in iterblocks(ip))

def states(ip: str) -> Generator[list[Block]]:
    blocks = list(iterblocks(ip))
    yield blocks

    free_block_indices = list(reversed([i for i, block in enumerate(blocks) if isinstance(block, FreeBlock)]))
    file_block_indices = [i for i, block in enumerate(blocks) if isinstance(block, FileBlock)]

    while file_block_indices:
        first_free_block_index = free_block_indices.pop()
        last_file_block_index = file_block_indices.pop()

        if first_free_block_index > last_file_block_index:
            break

        blocks[first_free_block_index] = blocks[last_file_block_index]
        blocks[last_file_block_index] = FreeBlock()

        yield blocks

def states_s(ip: str) -> str:
    return '\n'.join(
        ''.join(str(block) for block in blocks)
        for blocks in states(ip)
    )

def p1(ip: str) -> int:
    for blocks in states(ip):
        pass

    result = 0

    for i, block in enumerate(blocks):
        match block:
            case FileBlock(file_id):
                result += i * file_id
            case FreeBlock():
                pass
            case _:
                assert_never(block)

    return result