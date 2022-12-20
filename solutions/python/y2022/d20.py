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
    ('mix_stages_csv', '1,2,-3,3,-2,0,4;2,1,-3,3,-2,0,4;1,-3,2,3,-2,0,4;1,2,3,-2,-3,0,4;1,2,-2,-3,0,3,4;1,2,-3,0,3,4,-2;1,2,-3,0,3,4,-2;1,2,-3,4,0,3,-2'),
    ('grove_coords_csv', '4,-3,2'),
    ('p1', 3),
    ('p2', 0)
]), ('example2', '''\
4
0\
''', [
    ('mix_stages_csv', '4,0;4,0;4,0')
])]

@dataclass
class File:
    ls: list[int]

    def __init__(self: Self, ls: Iterable[int]) -> None:
        self.ls = list(ls)

    def __len__(self: Self) -> int:
        return len(self.ls)

    def __iter__(self: Self) -> Iterator[int]:
        yield from self.ls

    def __getitem__(self: Self, i: int | slice) -> int:
        len_ = len(self.ls)

        if isinstance(i, slice):
            assert i.start <= i.stop and i.step in (None, 1), i
            q1, r1 = divmod(i.start, len_)
            q2, r2 = divmod(i.stop, len_)

            if q1 == q2:
                return self.ls[r1:r2]
            elif q1 + 1 == q2:
                return self.ls[r1:] + self.ls[:r2]
            else:
                return self.ls[r1:] + self.ls * (q2 - q1 - 1) + self.ls[:r2]

        return self.ls[i % len_]

    def __setitem__(self: Self, i: int | slice, v: int | Iterable[int]) -> int:
        len_ = len(self.ls)

        if isinstance(i, slice):
            assert i.start <= i.stop and i.step in (None, 1), i
            assert not isinstance(v, int)
            v = list(v)
            vlen = len(v)
            assert vlen == i.stop - i.start
            q1, r1 = divmod(i.start, len_)
            q2, r2 = divmod(i.stop, len_)
            
            if q1 == q2:
                self.ls[r1:r2] = v
            elif q1 + 1 == q2:
                s = len_ - r1
                self.ls[r1:] = v[:s]
                #self.ls[:vlen - s] = v[s:]
                self.ls[:r2] = v[s:] # vlen - s = (stop - start) - (len_ - r1)
                                     #          = ( (q2 * len_ + r2) - (q1 * len_ + r1) ) - (len_ - r1)
                                     #          = ( len_ + r2 - r1 ) - ( len_ - r1 )
                                     #          = r2
            else:
                print(f'slicing from {i.start} to {i.stop}, len is {len_}')
                self.ls[r1:] = v[:len_ - r1]
                new_ls = None

                # for i in range(len_ - r1, vlen - r2, len_):
                #     vv = v[i:i + len_]
                #     print(i, vv)

                #     if new_ls is None:
                #         assert vv[r1:] == self.ls[r1:]
                #         new_ls = vv
                #     else:
                #         assert vv == new_ls

                #     self.ls[:] = vv

                # assert v[vlen - r2:] == self.ls[:r2]
                self.ls[:r2] = v[vlen - r2:]
                # print(self.ls)
        else:
            assert isinstance(v, int)
            self.ls[i % len_] = v

    def index(self: Self, v: int) -> int:
        return self.ls.index(v)

    def copy(self: Self) -> Self:
        return self.__class__(self.ls.copy())

    def __delitem__(self: Self, i: int) -> None:
        del self.ls[i % len(self)]

    def insert(self: Self, i: int, v: int) -> None:
        self.ls.insert(i, v)

def parse(ip: str) -> list[int]:
    return File(map(int, ip.splitlines()))

def mix_stages(f: File) -> Iterator[File]:
    fc = f.copy()
    yield fc

    f = fc.ls.copy()

    for v in fc.ls:
        i = f.index(v) # this doesn't work cuz thingies are unique
        new_i = i + v
        # print(f'{v=}, {i=}, {new_i=}')
        # print(f)
        # input()

        if new_i > i:
            for j in range(i, new_i):
                j0 = j % len(f)
                j1 = (j + 1) % len(f)
                f[j0], f[j1] = f[j1], f[j0]
        elif new_i < i:
            for j in range(i, new_i, -1):
                j0 = j % len(f)
                j1 = (j - 1) % len(f)
                f[j0], f[j1] = f[j1], f[j0]


    # # -11553 is wrong
    # # 1999 is too low
    # # 6841 is wrong

    # for v in fc.ls:
    #     # print()
    #     # print(v, f)
    #     i = f.index(v)
    #     new_i = (i + v) % len(f)
    #     # print(f'del at {i}, ins at {new_i}')
    #     # input()

    #     if i < new_i:

    #         # ..., f[i - 1], f[i], f[i + 1], ..., f[new_i - 1], f[new_i], ...
    #         del f[i]
    #         # print('ibt', f)
    #         # ..., f[i - 1], f[i + 1], ..., f[new_i - 1], f[new_i], ...
    #         f.insert(new_i - (v < 0), v)
    #         # ..., f[i - 1], f[i + 1], ..., f[new_i - 1], f[i], f[new_i], ...

    #         #f.insert(new_i + 1, v) # ?

    #         # sadly this approach doesn't work:
    #         # ..., f[i],     f[i + 1], ..., f[new_i - 1], f[new_i], ...
    #         # ..., f[i + 1], f[i + 2], ..., f[new_i],     f[i],     ... 
            
    #         #f[i], f[i + 1:new_i], f[new_i] = f[i + 1], f[i + 2:new_i + 1], v
            
    #         # because if you're shifting by more than length, e.g. if your list is just [4, 0], you get
    #         # ..., 4, 0, 4, 0, 4, 0, 4, _, 0, ...
    #         # ..., 4, 0, _, 0, 4, 0, 4, 4, 0, ...
    #         # which is incorrect: all the 4s have to shift at once
    #     elif new_i < i:
    #         # ..., f[new_i], f[new_i + 1], ..., f[i - 1], f[i], f[i + 1], ...
    #         del f[i]
    #         # print('ibt', f)
    #         # ..., f[new_i], f[new_i + 1], ..., f[i - 1], f[i + 1], ...
    #         if new_i == 0:
    #             # print('fuckleton wuckleton')
    #             new_i = len(f)
    #         f.insert(new_i + (v > 0), v)
    #         # ..., f[i], f[new_i], f[new_i + 1], ..., f[i - 1], f[i + 1], ...

    #         # ..., f[new_i], f[new_i + 1], ..., f[i - 1], f[i],     ...
    #         # ..., f[i],     f[new_i],     ..., f[i - 2], f[i - 1], ...
    #         # f[new_i], f[new_i + 1:i], f[i] = v, f[new_i:i - 1], f[i - 1]

        yield f.copy()

def mix_stages_csv(ip: str) -> str:
    ls = parse(ip)
    stages = list(mix_stages(ls))
    return ';'.join(','.join(map(str, stage)) for stage in stages)

def mix(f: list[int]) -> list[int]:
    for f in mix_stages(f):
        pass

    return f

def grove_coords(f: list[int]) -> list[int]:
    i = f.index(0)

    for k in (1000, 2000, 3000):
        yield f[(i + k) % len(f)]

def grove_coords_csv(ip: str) -> str:
    return ','.join(map(str, grove_coords(mix(parse(ip)))))

def p1(ip: str) -> int:
    return sum(grove_coords(mix(parse(ip))))

def p2(ip: str) -> int:
    return 0
