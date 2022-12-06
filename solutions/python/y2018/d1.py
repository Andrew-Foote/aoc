# adapted from solution written in 2018

import sys
from typing import Iterator
from utils import joinlines

test_inputs = [
    ('example', joinlines(['+1', '-2', '+3', '+1']), [
        ('freqs_csv', ','.join(['1', '-1', '2', '3'])),
        ('p1', '3'),
        ('freqs_until_repeat_csv', ','.join(['1', '-1', '2', '3', '4', '2'])),
        ('p2', '2')
    ]),
    ('example2', joinlines(['+1', '+1', '+1']), [('p1', '3')]),
    ('example3', joinlines(['+1', '+1', '-2']), [('p1', '0')]),
    ('example4', joinlines(['-1', '-2', '-3']), [('p1', '-6')]),
    ('example5', joinlines(['+1', '-1']), [('p2', '0')]),
    ('example6', joinlines(['+3', '+3', '+4', '-2', '-4']), [('p2', '10')]),
    ('example7', joinlines(['-6', '+3', '+8', '+5', '-6']), [('p2', '5')]),
    ('example8', joinlines(['+7', '+7', '-2', '-7', '-4']), [('p2', '14')])
]

def freqs(ip: str) -> Iterator[int]:
    freq = 0

    for line in ip.splitlines():
        freq += int(line)
        yield freq

def freqs_csv(ip: str) -> str:
    return ','.join(map(str, freqs(ip)))

def p1(ip: str) -> str:
    for freq in freqs(ip):
        pass

    return freq

def freqs_until_repeat(ip: str) -> Iterator[int]:
    lines = ip.splitlines()
    freq = 0
    freqs_reached = {0}

    while True:
        for line in lines:
            freq += int(line)
            yield freq

            if freq in freqs_reached:
                return

            freqs_reached.add(freq)

def freqs_until_repeat_csv(ip: str) -> str:
    return ','.join(map(str, freqs_until_repeat(ip)))

def p2(ip: str) -> str:
    for freq in freqs_until_repeat(ip):
        pass

    return freq