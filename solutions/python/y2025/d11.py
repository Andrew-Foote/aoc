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

    return sum(
        numpaths(graph, inter, end) for inter in graph[start]
    )

def p1(ip: str) -> int:
    # how many paths from you to out?
    # well there aren't any direct ones
    # number of length 3: can be determined by searching the outputs of you
    # (bbb and ccc), and seeing if any of those have out as an input
    #
    # number of paths from A to B
    # = (1, if (A, B) is an edge, 0 otherwise)
    #   + sum_(C such that (A, C) is an edge)
    #         (number of paths from C to B)
    return numpaths(parse(ip), 'you', 'out')