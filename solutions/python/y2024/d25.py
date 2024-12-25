from collections.abc import Generator
from dataclasses import dataclass
import itertools as it
from typing import assert_never

test_inputs = [('lock1', '''\
#####
.####
.####
.####
.#.#.
.#...
.....''', [
     ('lock_heights_csv', '0,5,3,4,3'),
 ]), ('key1', '''\
.....
#....
#....
#...#
#.#.#
#.###
#####''', [
    ('key_heights_csv', '5,0,2,1,3'),
]), ('example', '''\
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####''', [
    ('overlaps_csv', '0,5,3,4,3 and 4,3,4,0,2 ; 0,5,3,4,3 and 5,0,2,1,3 ; 1,2,0,5,3 and 5,0,2,1,3'),
    ('p1', 3)
])]

@dataclass(frozen=True, order=True)
class Lock:
    heights: tuple[int, ...]

@dataclass(frozen=True, order=True)
class Key:
    heights: tuple[int, ...]

def lock_heights(s: str) -> tuple[int, ...]:    
    lines = s.splitlines()
    assert len(lines) == 7
    assert all(len(line) == 5 for line in lines)
    result: list[int] = []

    for j in range(5):
        col = [line[j] for line in lines]
        result.append(col.index('.') - 1)

    return tuple(result)
    
def lock_heights_csv(ip: str) -> str:
    return ','.join(map(str, lock_heights(ip)))

def key_heights(s: str) -> tuple[int, ...]:    
    lines = s.splitlines()
    assert len(lines) == 7
    assert all(len(line) == 5 for line in lines)
    result: list[int] = []

    for j in range(5):
        col = [line[j] for line in reversed(lines)]
        result.append(col.index('.') - 1)

    return tuple(result)
    
def key_heights_csv(ip: str) -> str:
    return ','.join(map(str, key_heights(ip)))

def parse(ip: str) -> Generator[Lock | Key]:
    parts = ip.split('\n\n')

    for part in parts:
        if part[0] == '#': # lock
            yield Lock(lock_heights(part))
        elif part[-1] == '#': # key
            yield Key(key_heights(part))
        else:
            assert False, part

def overlaps(lock: Lock, key: Key) -> bool:
    return any(lh + kh > 5 for lh, kh in zip(lock.heights, key.heights))

def overlap_set(ip: str) -> Generator[tuple[Lock, Key]]:
    locks: list[Lock] = []
    keys: list[Key] = []

    for lk in parse(ip):
        match lk:
            case Lock():
                locks.append(lk)
            case Key():
                keys.append(lk)
            case _:
                assert_never(lk)

    for lock, key in it.product(locks, keys):
        if overlaps(lock, key):
            yield lock, key

def overlaps_csv(ip: str) -> str:
    ols = sorted(overlap_set(ip))
    bits: list[str] = []

    for lock, key in ols:
        bits.append(','.join(map(str, lock.heights)) + ' and ' + ','.join(map(str, key.heights)))

    return ' ; '.join(bits)

def p1(ip: str) -> int:
    locks: list[Lock] = []
    keys: list[Key] = []

    for lk in parse(ip):
        match lk:
            case Lock():
                locks.append(lk)
            case Key():
                keys.append(lk)
            case _:
                assert_never(lk)

    # total_pairs = len(locks) * len(keys)
    count = 0

    for lock, key in it.product(locks, keys):
        if not overlaps(lock, key):
            count += 1

    return count