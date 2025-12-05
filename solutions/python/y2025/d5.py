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
        ('p1', 3)
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