from dataclasses import dataclass

test_inputs = [('example', '''\
\
''', [
    ('p1', 0),
    ('p2', 0)
])]

def parse(ip: str) -> Iterator[int]:
    return ip.splitlines()

def p1(ip: str) -> int:
    return 0

def p2(ip: str) -> int:
    return 0