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
    ]),
    # https://old.reddit.com/r/adventofcode/comments/1pd118w/2025_day_3_part_2_what_are_the_edge_cases_my/
    ('extra_example', '''\
2232546378857275787561723292343835435343333776427842773354273372424413455462238746648634437374254318
2232323232236223322223321222232212221212222222222332111132223222222222322133213322323133322222332224
5345633566354453355546874555676462558526423364443535432344223165523377525665661379556365535642545245
3312322113352322342133434233342422313224135342333232234332332232313223352233232336232233533323364322''', [
        ('p2_largest_joltages_csv', '988887754318,633333333334,966642545245,653333364322')
    ])
]

def parse(ip: str) -> Iterator[list[int]]:
    for line in ip.splitlines():
        yield [int(d) for d in line]

def largest_joltage(bank: list[int]) -> int:
    largest_so_far: list[int] | None = None

    for i in range(len(bank)):
        d1 = bank[i]

        if largest_so_far is not None and d1 < largest_so_far[0]:
            continue

        for j in range(i + 1, len(bank)):
            d0 = bank[j]
            
            if largest_so_far is not None and d1 == largest_so_far[0] and d0 < largest_so_far[1]:
                continue
    
            largest_so_far = [d1, d0]

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
    cur_indices: list[int]
) -> list[int]:
    
    cur_digits = [bank[i] for i in cur_indices]

    if len(cur_indices) == 12:
        return cur_digits
    
    start = cur_indices[-1] + 1 if cur_indices else 0

    # this sorting was the key to making it efficient
    # i guess because it ensures that the if-condition here rules out as many
    # branches of the tree as possible
    for i in sorted(range(start, len(bank)), key=lambda i: -bank[i]):
        d = bank[i]

        if (
            largest_so_far is not None
            and cur_digits + [d] < largest_so_far[:i + 1]
        ):
            continue

        largest_so_far = find_largest_joltage(
            bank, largest_so_far, cur_indices + [i]
        )
        
    return largest_so_far

def largest_joltage_p2(bank: list[int]) -> int:
    largest = find_largest_joltage(bank, None, [])
    return fromdigits(largest)

def largest_joltages_p2(ip: str) -> Iterator[int]:
    for bank in parse(ip):
        yield largest_joltage_p2(bank)

def p2_largest_joltages_csv(ip: str) -> str:
    return ','.join(map(str, largest_joltages_p2(ip)))

def p2(ip: str) -> int:
    import time
    t0 = time.perf_counter_ns()
    r = sum(largest_joltages_p2(ip))
    t1 = time.perf_counter_ns()
    print((t1 - t0) / 1_000_000, 'milliseconds')
    return r