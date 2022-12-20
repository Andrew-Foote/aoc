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
    ('grove_coords_csv_p2', '811589153,2434767459,-1623178306'),
    ('p2', 1623178306)
]), ('example2', '''\
4
0\
''', [
    ('mix_stages_csv', '0,4;0,4;0,4')
])]

def parse(ip: str) -> Iterator[int]:
    return map(int, ip.splitlines())

def mix_stages(f: Iterable[int], mix_count: int=1) -> Iterator[list[int]]:
    vs = list(f)
    n = len(vs)
    js = list(range(n))

    i0 = vs.index(0)
    shifted_vs = [vs[i % n] for i in range(i0, i0 + n)]
    yield shifted_vs
    # print(shifted_vs)
    # input()

    for _ in range(mix_count):
        for j, v in enumerate(vs):
            j0 = js.index(j)
            j1 = j0 + v

            if j1 > j0:
                d = j1 - j0

                if d >= 2 * (n - 1):
                    j1 = j0 + (n - 1) + d % (n - 1)

                for i in range(j0, j1):
                    k0 = i % n
                    k1 = (i + 1) % n
                    js[k0], js[k1] = js[k1], js[k0]

            elif j1 < j0:
                d = j0 - j1

                if d >= 2 * (n - 1):
                    j1 = j0 - (n - 1) - d % (n - 1)

                for i in range(j0, j1, -1):
                    k0 = i % n
                    k1 = (i - 1) % n
                    js[k0], js[k1] = js[k1], js[k0]

            ii0 = js.index(i0)
            mixed_and_shifted_vs = [vs[js[i % n]] for i in range(ii0, ii0 + n)]
            # print(f'{mixed_and_shifted_vs=}')
            # input()
            yield mixed_and_shifted_vs

        # print(mixed_and_shifted_vs)
        # input()

def mix_stages_csv(ip: str) -> str:
    ls = parse(ip)
    stages = list(mix_stages(ls))
    return ';'.join(','.join(map(str, stage)) for stage in stages)

def mix(f: Iterable[int], count: int=1) -> list[int]:
    for vs in mix_stages(f, count):
        pass

    return vs

def grove_coords(vs: list[int]) -> list[int]:
    for k in (1000, 2000, 3000):
        yield vs[k % len(vs)]

def grove_coords_csv(ip: str) -> str:
    return ','.join(map(str, grove_coords(mix(parse(ip)))))

def p1(ip: str) -> int:
    return sum(grove_coords(mix(parse(ip))))

def grove_coords_csv_p2(ip: str) -> str:
    KEY = 811589153
    numbers = [KEY * number for number in parse(ip)]
    return ','.join(map(str, grove_coords(mix(numbers, 10))))

def p2(ip: str) -> int:
    KEY = 811589153
    numbers = [KEY * number for number in parse(ip)]
    return sum(grove_coords(mix(numbers, 10)))

