import functools as ft

test_inputs = [
    ('example', '''\
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out''', [
        ('p1', 5)
    ]),
    ('example2', '''\
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out''', [
        ('p2', 2)
    ])
]

def parse(ip) -> dict[str, set[str]]:
    result: dict[str, set[str]] = {}
 
    for line in ip.splitlines():
        key, vals_str = line.split(':')
        vals = {val.strip() for val in vals_str.split()}
        result[key] = vals

    return result

def numpaths(graph: dict[str, set[str]], start: str, end: str) -> int:
    if start == end:
        return 1

    return sum(numpaths(graph, inter, end) for inter in graph[start])

def p1(ip: str) -> int:
    return numpaths(parse(ip), 'you', 'out')

def p2(ip: str) -> int:
    graph = parse(ip)
    end = 'out'

    @ft.cache
    def numpaths_p2(start: str, inters: frozenset[str]) -> int:
        """Number of paths from start to end that visit each node in inters."""
        if start == end:
            return inters.issubset({start})
        
        result: int = 0

        for inter in graph[start]:
            result += numpaths_p2(inter, inters - frozenset({inter}))

        return result

    return numpaths_p2('svr', frozenset({'dac', 'fft'}))