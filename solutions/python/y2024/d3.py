import re

test_inputs = [
    ('example', '''\
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))''', [
        ('p1', 161),
    ])
]

def p1(ip: str) -> int:
    return sum(
        int(a) * int(b) for a, b in re.findall(r'mul\((\d+),(\d+)\)', ip)
    )