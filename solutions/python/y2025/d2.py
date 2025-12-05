from collections.abc import Iterator
import math 
from solutions.python.lib.digits import digit_count

test_inputs = [
    ('example', '''\
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124''', [
        ('p1_invalid_ids_csv', '11,22;99;1010;1188511885;222222;;446446;38593859;;;'),
        ('p1', 1227775554),
        ('p2_invalid_ids_csv', '11,22;99,111;999,1010;1188511885;222222;;446446;38593859;565656;824824824;2121212121'),
        ('p2', 4174379265)
    ]),
    ('example2', '''\
11-42,95-115,998-7012,1188511880-2188511890,222220-222224,1698522-1698528,446443-646449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2321212124''', [
        ('p1', 21327161532716),
        ('p2', 21346784611163)
    ]),
    ('example3', '''\
11-42,95-115,998-7012,222220-222224,446443-646449,1698522-1698528,38593856-38593862,824824821-824824827,1188511880-2321212124,202001202277-532532532530''', [
        ('p1', 121412594604227157),
        ('p2', 122614329477263799),
    ]),
]

def parse(ip: str) -> Iterator[tuple[int, int]]:
    ip = ip.replace('\n', '')
    
    for r in ip.split(','):
        a, b = r.split('-')
        yield int(a), int(b)

# The invalid IDs are the positive integers with decimal expansions of the form
# 
#   d_(n - 1) ... d_0 d_(n - 1) ... d_0,
#
# where n is a positive integer.
# 
# Any such integer has an even number of digits (namely 2n). It can also be 
# written as
# 
#   d_(n - 1) * 10^(2n - 1) + ... + d_0 * 10^n 
#     + d_(n - 1) * 10^(n - 1) + ... + d_0
#   = (d(n - 1) * 10^(n - 1) + ... + d_0) * 10^n
#     + (d_(n - 1) * 10^(n - 1) + ... + d_0)
#   = r * 10^n + r
#   = r(10^n + 1),
#
# where r is the positive integer with decimal expansion d_(n - 1) ... d_0. Thus
# it is divisible by 10^n + 1. Note that r has exactly n digits.
# 
# Conversely, for any positive integer x, if there are positive integers n and
# r such that r has n digits and x = r(10^n + 1), then x is an invalid ID (with
# 2n digits).
# 
# The problem is to find all the invalid IDs within a given inclusive range
# [a, b]. We could just check each integer within the range, but it will be
# more efficient to consider the range of possible numbers of digits of r. Since
# the ID will have twice as many digits as r, we'll have A <= 2r <= B, where
# where A is the number of digits of a, and B is the number of digits of b.
# So the possible numbers of digits of r are the elements of
# [ceil(A/2), floor(B/2)]. Given an n within this range, the possible values of
# r will be all multiples of y = 10^n + 1 that do indeed have n digits. To find
# these we can search through the range [ceil(a/y), floor(b/y)], checking for 
# each member whether it does indeed have n digits.

def p1_invalid_ids(ip: str) -> Iterator[list[int]]:
    for a, b in parse(ip):
        A = digit_count(a)
        B = digit_count(b)

        pids = set()

        for n in range(math.ceil(A/2), math.floor(B/2) + 1):
            y = 10 ** n + 1
                
            for r in range(math.ceil(a/y), math.floor(b/y) + 1):
                if digit_count(r) == n:
                    pids.add(r * y)

        yield sorted(pids)

def p1_invalid_ids_csv(ip: str) -> str:
    return ';'.join(','.join(map(str, pids)) for pids in p1_invalid_ids(ip))

def p1(ip: str) -> int:
    return sum(sum(pids) for pids in p1_invalid_ids(ip))

def p2_invalid_ids_csv(ip: str) -> str:
    return ';'.join(','.join(map(str, pids)) for pids in p2_invalid_ids(ip))

# For part 2, the invalid IDs are the positive integers with decimal expansions
# consisting of a block of digits d_(n - 1), ..., d_0 repeated m times, for 
# some integer m >= 2.
# 
# Any such integer has a number of digits divisible by m (namely mn). It can
# also be written as
#
#   r * 10^((m - 1)n) + ... + r * 10^n + a
#   = r((10^n)^(m - 1) + ... + 10^n + 1)
#   = r((10^n)^m - 1)/(10^n - 1)
#   = r(10^mn - 1)/(10^n - 1),
#
# where r is the positive integer with decimal expansion d_(n - 1) ... d_0. Thus
# it is divisible by (10^mn - 1)/(10^n - 1).
#
# Conversely, for any positive integer x, if there are positive integers m and n
# such that m >= 2, r has n digits and x = a(10^mn - 1)/(10^n - 1), then x is an
# invalid ID (within mn digits).
# 
# The problem is to find all the invalid IDs within a given inclusive range
# [a, b]. We could just check each integer within the range, but it will be
# more efficient to consider the range of possible numbers of digits of r. Given
# a choice of m and n, the ID itself will have mn digits, and since it will
# belong to [a, b] we must have A <= mn <= B where A is the number of digits of 
# a, and B is the number of digits of b. This implies m <= B, and we also know
# that 2 <= m so we'll have 2 <= m <= B. So we can just search through the range
# [2, B] for possible values of m. Then, for a given choice of m, n must
# satisfy A/m <= n <= B/m, so we search through [ceil(A/m), floor(B/m)]. Then,
# given m and n, the possible values of r are all multiples of 
# y = (10^mn - 1)/(10^n - 1) that have n digits. To find these we can search
# through the range [ceil(a/y), floor(b/y)].

def p2_invalid_ids(ip: str) -> Iterator[list[int]]:
    for a, b in parse(ip):
        A = digit_count(a)
        B = digit_count(b)

        pids = set()

        for m in range(2, B + 1):
            for n in range(math.ceil(A/m), math.floor(B/m) + 1):
                y = (10 ** (m * n) - 1) // (10 ** n - 1)
                
                for r in range(math.ceil(a/y), math.floor(b/y) + 1):
                    if digit_count(r) == n:
                        pids.add(r * y)

        yield sorted(pids)

def p2(ip: str) -> int:
    return sum(sum(pids) for pids in p2_invalid_ids(ip))