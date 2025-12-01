from collections.abc import Iterator

test_inputs = [
    ('example', '''\
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82''', [
        ('dial_trace_csv', '82,52,0,95,55,0,99,0,14,32'),
        ('p1', 3),
        ('dial_trace_p2_csv', '82,1;52,0;0,1;95,0;55,1;0,1;99,0;0,1;14,0;32,1'),
        ('p2', 6),
    ]),
    ('example2', '''\
R1000''', [
        ('dial_trace_p2_csv', '50,10')
    ])
]

def parse(ip: str) -> Iterator[tuple[int, int]]:
    for line in ip.splitlines():
        sign = {'L': -1, 'R': 1}[line[0]]
        amount = int(line[1:])
        yield sign, amount

def dial_trace(ip: str) -> Iterator[int]:
    dial = 50

    for sign, amount in parse(ip):
        dial = (dial + sign * amount) % 100
        yield dial

def dial_trace_csv(ip: str) -> str:
    return ','.join(map(str, dial_trace(ip)))

def p1(ip: str) -> int:
    return sum(1 for dial in dial_trace(ip) if dial == 0)

def dial_trace_p2(ip: str) -> Iterator[tuple[int, int]]:
    dial = 50

    for sign, amount in parse(ip):
        change = sign * amount

        # Number of times dial arrives at 0 during the change
        # = number of integers in the inclusive range with endpoints
        #   dial + sign, dial + change which are divisible by 100
        #
        #  (this will be [dial + sign, dial + change] if sign == 1, and
        #  [dial + change, dial + sign] if sign == -1)
        # 
        # In general, the number of integers in the inclusive range [0, x]
        # divisible by 100 is floor(x / 100) + 1.
        #
        # So the number of integers in the inclusive range [a, x] divisible by
        # 100 will be floor(x / 100) + 1 minus the number in [0, a) which are
        # divisible by 100. The latter will be floor(a / 100) + 1, minus 1 if
        # a is divisible by 100.

        match sign:
            case 1:
                a, x = dial + sign, dial + change
            case -1:
                a, x = dial + change, dial + sign
            case _:
                assert False

        zero_count = x // 100 + 1 - (a // 100 + 1 - (a % 100 == 0))
        dial = (dial + change) % 100
        yield dial, zero_count

def dial_trace_p2_csv(ip: str) -> str:
    return ';'.join(
        f'{dial},{zero_count}' for dial, zero_count in dial_trace_p2(ip)
    )

def p2(ip: str) -> int:
    return sum(zero_count for _, zero_count in dial_trace_p2(ip))