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

    return sum(
        numpaths(graph, inter, end) for inter in graph[start]
    )

def numpaths_p2(graph: dict[str, set[str]], start: str, end: str, inters: set[str]) -> int:
    if start == end:
        return not inters
    
    result: int = 0

    for inter in graph[start]:
        if inter in inters:
            result += numpaths_p2(graph, inter, end, inters - {inter})
        else:
            result += numpaths_p2(graph, inter, end, inters)

    return result

def p1(ip: str) -> int:
    # how many paths from you to out?
    # well there aren't any direct ones
    # number of length 3: can be determined by searching the outputs of you
    # (bbb and ccc), and seeing if any of those have out as an input
    #
    # number of paths from A to B
    # = 1, if A = B
    # = sum_(C such that (A, C) is an edge)
    #         (number of paths from C to B),  otherwise
    return numpaths(parse(ip), 'you', 'out')

def p2(ip: str) -> int:
    graph = parse(ip)
    # need number of paths from svr to out that visit dac and fft
    # i guess we can make a new graph where the nodes contain
    # info about whether dac and fft have already been visited
    # so for each node in the original graph

    # number of paths from A to B that visit C, given boolean indicating whether
    #  C was already visited
    # = (1, if A = B and C was already visited)
    # = (0, if A = B and C wasn't already visited)
    # = sum_(D such that (A, D) is an edge)
    #       IF D = C:
    #           number of paths from C to B with C already visited
    #       IF D != C:
    #           number of paths from D to B with C already visited matching
    #              orig value
    return numpaths_p2(parse(ip), 'svr', 'out', {'dac', 'fft'})