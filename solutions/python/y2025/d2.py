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

def is_valid_p2(pid: str) -> bool:
    pids_to_debug = () #('565656', '2121212121') 
    debug = lambda s: print(s) if pid in pids_to_debug else None 
    debug(f'CHECKING WHETHER {pid} IS VALID')

    for part_count in range(2, len(pid) + 1):
        debug(f'  {part_count=}')

        if len(pid) % part_count:
            debug(f' cannae be split evenly for this part count')
            continue
        
        part_size = len(pid) // part_count
        prev_part = None
        debug(f'  {part_size=}, {prev_part=}; loop begins now')

        for k in range(part_count):
            next_part = pid[k * part_size:(k + 1) * part_size]
            debug(f'  {k=}, {next_part=}')

            if prev_part is None:
                prev_part = next_part
            elif prev_part != next_part:
                debug('  parts differ for this part count')
                break
        else:
            debug('  invalid because parts same')
            return False
        
    return True

def p2_invalid_ids(ip: str) -> Iterator[list[int]]:
    for a, b in parse(ip):
        pids = []

        for x in range(a, b + 1):
            if not is_valid_p2(str(x)):
                pids.append(x)

        yield pids

def p2_invalid_ids_csv(ip: str) -> str:
    return ';'.join(','.join(map(str, pids)) for pids in p2_invalid_ids(ip))

def p2(ip: str) -> int:
    s = 0

    for pids in p2_invalid_ids(ip):
        s += sum(pids)
    
    return s