from dataclasses import dataclass

test_inputs = [
    ('example', '''\
3-5
10-14
16-20
12-18

1
5
8
11
17
32''', [
        ('p1', 3),
        ('p2', 14),
    ])
]

@dataclass
class Database:
    fresh: list[range]
    available: list[int]

    def is_fresh(self, ingredient: int) -> bool:
        for r in self.fresh:
            if ingredient in r: 
                return True
            
        return False

def parse(ip) -> Database:
    parts = ip.split('\n\n')
    range_strings = parts[0].splitlines()
    ranges = []

    for range_string in range_strings:
        lb, ub = map(int, range_string.split('-'))
        ranges.append(range(lb, ub + 1))
    
    available = [int(i) for i in parts[1].splitlines()]
    return Database(ranges, available)

def p1(ip) -> int:
    db = parse(ip)
    
    return sum(
        1 for ingredient in db.available
        if db.is_fresh(ingredient)
    )

# K(A u B_1 u ... u B_n) = K(A) + K(B_1 u ... u B_n)
#                          - K(A n (B_1 u ... u B_n))
# K(A n (B_1 u ... u B_n)) = K((A n B_1) u ... u (A n B_n))

def range_intersect(r1: range, r2: range) -> range:
    start = max(r1.start, r2.start)
    stop = min(r1.stop, r2.stop)
    return range(start, stop)

import functools as ft

@ft.lru_cache
def total_size(*rs: range) -> int:
    #print(f'total_size({rs}) = ', end='')
    match rs:
        case []:
            #print('0')
            return 0
        case [r]:
            #print(f'{max(0, r.stop - r.start)}')
            return max(0, r.stop - r.start)
        case [r1, *r2s]:
            r3s = [range_intersect(r1, r2) for r2 in r2s]
            #print(f'total_size(r1) + total_size(*{r2s}) - total_size(*{r3s})')
            return (
                total_size(r1) + total_size(*r2s) - total_size(*r3s)
            )
        case _:
            assert False

def p2(ip) -> int:
    db = parse(ip)
    return total_size(*db.fresh)
