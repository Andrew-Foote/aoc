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
    ]),
]

def dial_trace(ip: str) -> Iterator[int]:
    dial = 50

    for line in ip.splitlines():
        sign = {'L': -1, 'R': 1}[line[0]]
        amount = int(line[1:])
        dial = (dial + sign * amount) % 100
        yield dial

def dial_trace_csv(ip: str) -> str:
    return ','.join(map(str, dial_trace(ip)))

def p1(ip: str) -> int:
    return sum(1 for dial in dial_trace(ip) if dial == 0)