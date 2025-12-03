from collections.abc import Iterator
from solutions.python.lib.digits import int_from_digits_leading_first as fromdigits
import itertools as it

test_inputs = [
    ('example', '''\
987654321111111
811111111111119
234234234234278
818181911112111''', [
        ('p1_largest_joltages_csv', '98,89,78,92'),
        ('p1', 357),
        ('p2_largest_joltages_csv', '987654321111,811111111119,434234234278,888911112111'),
        ('p2', 3121910778619),
    ]),
    ('smol_example', '''\
987654321111111
''', [
        ('p2_largest_joltages_csv', '987654321111'),
    ])
]

def parse(ip: str) -> Iterator[list[int]]:
    for line in ip.splitlines():
        yield [int(d) for d in line]

def largest_joltage(bank: list[int]) -> int:
    debug = False
    debugp = lambda s: print(s) if debug else None
    debugp(f'{bank=}')
    largest_so_far: list[int] | None = None

    for i in range(len(bank)):
        d1 = bank[i]
        debugp(f'  {largest_so_far=}, {i=}, {d1=}')

        if largest_so_far is not None and d1 < largest_so_far[0]:
            debugp(f'  too small')
            continue

        for j in range(i + 1, len(bank)):
            d0 = bank[j]
            debugp(f'     {largest_so_far=}, {i=}, {j=}, {d1=}, {d0=}')
            
            if largest_so_far is not None and d1 == largest_so_far[0] and d0 < largest_so_far[1]:
                debugp(f'    too small')
                continue
    
            largest_so_far = [d1, d0]
            debugp(f'   updating largest_so_far')

    return fromdigits(largest_so_far)

def largest_joltages(ip: str) -> Iterator[int]:
    for bank in parse(ip):
        yield largest_joltage(bank)

def p1_largest_joltages_csv(ip: str) -> str:
    return ','.join(map(str, largest_joltages(ip)))

def p1(ip: str) -> int:
    return sum(largest_joltages(ip))

# Find the largest possible joltage output for the given bank, given a set of
# digits that have already been turned on, if we are only allowed to turn on
# digits after the ones that have already been turned on.
def find_largest_joltage(
    bank: list[int],
    largest_so_far: list[int] | None, 
    cur_indices: list[int],
    debug: bool,
    nesting: int=1
) -> list[int]:
    
    debugp = lambda s: print(s) if debug else None
    cur_digits = [bank[i] for i in cur_indices]

    if len(cur_indices) == 12:
        debugp(' ' * (nesting - 1) + f'gfdlkld {cur_digits=}')
        if debug: input()
        return cur_digits
    
    start = cur_indices[-1] + 1 if cur_indices else 0
    debugp(' ' * (nesting - 1) + f'glmp({largest_so_far=}, {cur_indices=}, {cur_digits=}, {start=})')

    # this sorting was the key to making it efficient
    # i guess because it ensures that the if-condition here rules out as many
    # branches of the tree as possible
    for i in sorted(range(start, len(bank)), key=lambda i: -bank[i]):
        d = bank[i]
        debugp(' ' * nesting + f'{largest_so_far=}, {cur_indices=}, {i=}, {cur_digits=}, {d=}')

        if (
            largest_so_far is not None
            and cur_digits + [d] < largest_so_far[:i + 1]
        ):
            debugp(' ' * nesting + f'  too small')
            if debug: input()
            continue

        largest_so_far = find_largest_joltage(
            bank, largest_so_far, cur_indices + [i], debug, nesting + 1
        )
        
        debugp(' ' * nesting + f'   updating largest_so_far')

    return largest_so_far

def largest_joltage_p2(bank: list[int]) -> int:
    debug = False 
    debugp = lambda s: print(s) if debug else None
    debugp(f'{bank=}')

    largest = find_largest_joltage(bank, None, [], debug)
    return fromdigits(largest)

def largest_joltages_p2(ip: str) -> Iterator[int]:
    for bank in parse(ip):
        yield largest_joltage_p2(bank)

def p2_largest_joltages_csv(ip: str) -> str:
    return ','.join(map(str, largest_joltages_p2(ip)))

def p2(ip: str) -> int:
    return sum(largest_joltages_p2(ip))