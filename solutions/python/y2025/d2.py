from collections.abc import Iterator

test_inputs = [
    ('example', '''\
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124''', [
        ('p1', 1227775554),
        ('p2_invalid_ids_csv', '11,22;99,111;999,1010;1188511885;222222;;446446;38593859;565656;824824824;2121212121'),
        ('p2', 4174379265)
    ]),
]

def parse(ip: str) -> Iterator[tuple[int, int]]:
    ip = ip.replace('\n', '')
    
    for r in ip.split(','):
        a, b = r.split('-')
        yield int(a), int(b)

# An ID is invalid if it is of the form
# 
#   d_(n - 1) ... d_0 d_(n - 1) ... d_0
#
# for some positive integer n. If we interpret this as a decimal expansion then
# the number represented by it is
# 
#   d_(n - 1) * 10^(2n - 1) + ... + d_0 * 10^n 
#     + d_(n - 1) * 10^(n - 1) + ... + d_0
#   = (d(n - 1) * 10^(n - 1) + ... + d_0) * 10^n
#     + (d_(n - 1) * 10^(n - 1) + ... + d_0)
#   = r * 10^n + r
#   = r(10^n + 1),
#
# where r is the number with decimal expansion d_(n - 1) ... d_0. And since IDs
# can't have leading zeros, we have d_(n - 1) != 0 meaning a has exactly n 
# digits, i.e. 10^(n - 1) <= a < 10^n.
#
# Conversely, for any two positive integers a and n, if 10^(n - 1) <= a < 10^n,
# then a(10^n + 1) = a * 10^n + a, and the leading digit of a is nonzero, so 
# the decimal expansion of a(10^n + 1) will be an invalid ID.
#
# Our problem is to find the invalid IDs within a given range. In other words,
# given positive integers a, b, what are the integers x such that a <= x <= b,
# and there are positive integers n with 10^n + 1 | x and 10^(n - 1) < x < 10^n?
# Well, any such x will have 10^n + 1 <= x which
#
# Well, if q(10^n + 1) <= b then 

def is_valid(pid: str) -> bool:
    if len(pid) % 2:
        return True
    
    n = len(pid) // 2

    return int(pid) % (10 ** n + 1)

    return pid[:n] != pid[n:]

def p1(ip: str) -> int:
    s = 0

    for a, b in parse(ip):
        for x in range(a, b + 1):
            if not is_valid(str(x)):
                s += x

    return s

def is_valid_p2(pid: str) -> bool:
    pids_to_debug = ()
    debug = lambda s: print(s) if pid in pids_to_debug else None 
    debug(f'CHECKING WHETHER {pid} IS VALID')

    for part_count in range(2, len(pid) + 1):
        debug(f'  {part_count=}')

        if len(pid) % part_count:
            debug(f' cannae be split evenly for this part count')
            continue
        
        part_size = len(pid) // part_count
        debug(f'  {part_size=}')
    
        x = 10 ** (part_count * part_size) - 1
        y = 10 ** part_size - 1

        if int(pid) % (x // y) == 0:
            return False

    return True

def p2_invalid_ids_old(ip: str) -> Iterator[list[int]]:
    for a, b in parse(ip):
        pids = []

        for x in range(a, b + 1):
            if not is_valid_p2(str(x)):
                pids.append(x)

        yield pids

def p2_invalid_ids_csv(ip: str) -> str:
    return ';'.join(','.join(map(str, pids)) for pids in p2_invalid_ids(ip))

# An ID is invalid if there is a sequence of digits d_(n - 1), ..., d_0 such
# that the ID is just that sequence repeated m times, for some integer m >= 2.
# If this is the case, then letting a be the number represented by
# d_(n - 1) ... d_0, so that
#
#   a = d_(n - 1) * 10^(n - 1) + ... + d_1 * 10 + d_0,
#
# then the invalid ID will represent the number
#
#   a * 10^((m - 1)n) + ... + a * 10^n + a
#   = a((10^n)^(m - 1) + ... + 10^n + 1)
#   = a((10^n)^m - 1)/(10^n - 1)
#   = a(10^mn - 1)/(10^n - 1).
#
# Conversely, any number of the form a(10^mn - 1)/(10^n - 1), where m is an
# integer >= 2, n is a positive integer, and a is positive integer with exactly 
# n digits, corresponds to an invalid ID. (The "exactly n digits" condition is 
# necessary because IDs can't have leading zeros.)
#
# So our problem is to find all numbers of this form within a given inclusive
# range [A, B]. Since a number of this form can also be written as 
#
#   a * 10^((m - 1)n) + ... + a * 10^n + a,
#
# it is >= a * 10^((m - 1)n), and since a has exactly n digits, so that
# a >= 10^(n - 1), it follows that the number is >= 10^(mn - 1). So if the
# number is <= B it follows that 10^(mn - 1) <= B and hence mn - 1 <= log_10 B.
# So we can limit our search to pairs (m, n) with mn <= log_10 B + 1. Given
# such a pair, we can calculate x = 10^mn - 1, y = 10^n - 1, and then we want
# to find a with A <= ax/y <= B, i.e. Ay <= ax <= By, i.e.
#  ceil(Ay/x) <= a <= floor(By/x). But we also want to ensure ax/y is an 
# integer...
#  Ay <= ax <= By
#  ceil(Ay) <= ax <= 
# 
#  x = (10^mn - 1)/(10^n - 1) and then just take
# all the multiples of x within [A, B] that are integers
# A <= kx <= B
# A/x <= k <= B/x

import math 

def p2_invalid_ids(ip: str) -> Iterator[list[int]]:
    for a, b in parse(ip):
        pids = []
        pid_set = set()

        for p in range(0, math.floor(math.log10(b) + 1) + 1):
            for m in range(2, p + 1):
                n = p // m
                x = 10 ** (m * n) - 1
                y = 10 ** n - 1

                for k in range(math.ceil(a * y / x), math.floor(b * y / x) + 1):
                    if 10 ** (n - 1) <= k < 10 ** n and (k * x) % y == 0:
                        pid = int(k * x / y)

                        if pid not in pid_set:
                            pid_set.add(pid)
                            pids.append(pid)

        yield pids

def p2(ip: str) -> int:
    s = 0

    for pids in p2_invalid_ids(ip):
        s += sum(pids)
    
    return s