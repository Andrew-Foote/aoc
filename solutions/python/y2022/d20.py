from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from typing import Self

test_inputs = [('example', '''\
1
2
-3
3
-2
0
4\
''', [
    ('mix_stages_csv', ';'.join([
        '0,4,1,2,-3,3,-2',
        '0,4,2,1,-3,3,-2',
        '0,4,1,-3,2,3,-2',
        '0,4,1,2,3,-2,-3',
        '0,3,4,1,2,-2,-3',
        '0,3,4,-2,1,2,-3',
        '0,3,4,-2,1,2,-3',
        '0,3,-2,1,2,-3,4'
    ])),
    ('grove_coords_csv', '4,-3,2'),
    ('p1', 3),
    ('p2', 0)
]), ('example2', '''\
4
0\
''', [
    ('mix_stages_csv', '0,4;0,4;0,4')
])]

def parse(ip: str) -> Iterator[int]:
    return map(int, ip.splitlines())

def mix_stages(f: Iterable[int]) -> Iterator[list[int]]:
    vs = list(f)
    n = len(vs)
    js = list(range(n))

    i0 = vs.index(0)
    shifted_vs = [vs[i % n] for i in range(i0, i0 + n)]
    # print(f'{shifted_vs=}')
    # input()
    yield shifted_vs

    for j, v in enumerate(vs):
        j0 = js.index(j)
        j1 = j0 + v

        if j1 > j0:
            for i in range(j0, j1):
                k0 = i % n
                k1 = (i + 1) % n
                js[k0], js[k1] = js[k1], js[k0]
        elif j1 < j0:
            for i in range(j0, j1, -1):
                k0 = i % n
                k1 = (i - 1) % n
                js[k0], js[k1] = js[k1], js[k0]

        ii0 = js.index(i0)
        mixed_and_shifted_vs = [vs[js[i % n]] for i in range(ii0, ii0 + n)]
        # print(f'{mixed_and_shifted_vs=}')
        # input()
        yield mixed_and_shifted_vs

def mix_stages_csv(ip: str) -> str:
    ls = parse(ip)
    stages = list(mix_stages(ls))
    return ';'.join(','.join(map(str, stage)) for stage in stages)

def mix(f: Iterable[int]) -> list[int]:
    for vs in mix_stages(f):
        pass

    return vs

def grove_coords(vs: list[int]) -> list[int]:
    for k in (1000, 2000, 3000):
        yield vs[k % len(vs)]

def grove_coords_csv(ip: str) -> str:
    return ','.join(map(str, grove_coords(mix(parse(ip)))))

def p1(ip: str) -> int:
    return sum(grove_coords(mix(parse(ip))))

def p2(ip: str) -> int:
    return 0
