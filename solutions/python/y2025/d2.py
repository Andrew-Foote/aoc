from collections.abc import Iterator

test_inputs = [
    ('example', '''\
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124''', [
        ('p1', 1227775554),
    ]),
]

def parse(ip: str) -> Iterator[tuple[int, int]]:
    ip = ip.replace('\n', '')
    
    for r in ip.split(','):
        a, b = r.split('-')
        yield int(a), int(b)

def is_valid(pid: str) -> bool:
    if len(pid) % 2:
        return True
    
    n = len(pid) // 2
    return pid[:n] != pid[n:]

def p1(ip: str) -> int:
    s = 0

    for a, b in parse(ip):
        for x in range(a, b + 1):
            if not is_valid(str(x)):
                s += x

    return s