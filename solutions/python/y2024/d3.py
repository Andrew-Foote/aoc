import re

test_inputs = [
    ('example', '''\
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))''', [
        ('p1', 161),
    ]),
    ('example2', '''\
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))''', [
        ('p2', 48),
    ])
]

def p1(ip: str) -> int:
    return sum(
        int(a) * int(b) for a, b in re.findall(r'mul\((\d+),(\d+)\)', ip)
    )

def p2(ip: str) -> int:
    pat = r"mul\((\d+),(\d+)\)|(do)\(\)|(don't)\(\)"
    enabled = True
    result = 0

    for l, r, do, dont in re.findall(pat, ip):
        is_mul = l or r
        is_do = do == 'do'
        is_dont = dont == "don't"
        assert not (is_mul and is_do)
        assert not (is_mul and is_dont)
        assert not (is_do and is_dont)

        if is_mul:
            if enabled:
                result += int(l) * int(r)
        elif is_do:
            enabled = True
        elif is_dont:
            enabled = False
        else:
            assert False

    return result
