from collections.abc import Generator
from dataclasses import dataclass
import itertools as it
from typing import assert_never

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
        ('p2_state_pics', '''\
00...111...2...333.44.5555.6666.777.888899
0099.111...2...333.44.5555.6666.777.8888..
0099.1117772...333.44.5555.6666.....8888..
0099.111777244.333....5555.6666.....8888..
00992111777.44.333....5555.6666.....8888..'''),
        ('p2', 2858),
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

    free_block_indices = list(reversed([
        i for i, block in enumerate(blocks) if isinstance(block, FreeBlock)
    ]))
    
    file_block_indices = [
        i for i, block in enumerate(blocks) if isinstance(block, FileBlock)
    ]

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

@dataclass(frozen=True)
class File:
    id: int
    size: int
    padding: int # specificially right-padding

def state_pic(state: list[File]) -> str:
    result_chars: list[str] = []

    for file in state:
        result_chars.append(str(file.id) * file.size + '.' * file.padding)

    return ''.join(result_chars)

def p2_parse(ip: str) -> Generator[File]:
    for id_, batch in enumerate(it.batched(ip.strip(), 2)):
        match batch:
            case (size_s, padding_s):
                size = int(size_s.strip())
                padding = int(padding_s.strip())
            case (size_s,):
                size = int(size_s.strip())
                padding = 0
            case _:
                assert False

        yield File(id_, size, padding)

# Maybe could be made more efficient using linked lists but it actually only
# takes around 5 seconds
def p2_states(ip: str) -> Generator[list[File]]:
    files = list(p2_parse(ip))
    yield files
    # print()
    
    for file_id, file in reversed(list(enumerate(files[1:], start=1))):
        # print(f'OUTER LOOP ITERATION BEGIN file_id={file_id}, file={file}')
        # print(f'  CUR STATE: {state_pic(files)}')
        i = 0

        for i, file in enumerate(files):
            if file.id == file_id:#
                break

        # print(f'  FILE FOUND AT INDEX {i}')

        size = file.size
        file_before = files[i - 1]
        
        # print(f'  SIZE = {size}, FILE_BEFORE = {file_before}')
        
        for j, new_file_before in enumerate(files[:i]):
            # print(f'  CONSIDERING {new_file_before} AT {j}')

            space = new_file_before.padding

            if size <= space:
                # print(f'  FIT POSSIBLE!!!')

                files[i - 1:i + 1] = [File(
                    file_before.id,
                    file_before.size,
                    file_before.padding + size + file.padding
                )]

                files[j:j + 1] = [
                    File(new_file_before.id, new_file_before.size, 0),
                    File(file.id, size, space - size)
                ]

                yield files
                break

def p2_state_pics(ip: str) -> str:
    return '\n'.join(state_pic(state) for state in p2_states(ip))

def p2(ip: str) -> int:
    for state in p2_states(ip):
        pass
    
    result = 0
    pos = 0

    for file in state:
        #   file.id * pos
        # + file.id * (pos + 1)
        # + ...
        # + file.id * (pos + file.size - 1)
        # = file.id * file.size * (2 * pos + file.size - 1) / 2
        result += file.id * file.size * (2 * pos + file.size - 1) // 2
        pos += file.size + file.padding

    return result
