from collections import defaultdict
from collections.abc import Generator

test_inputs = [('example', '''\
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn''', [
    ('interconnected_groups_lines', '''\
aq,cg,yn
aq,vc,wq
co,de,ka
co,de,ta
co,ka,ta
de,ka,ta
kh,qp,ub
qp,td,wh
tb,vc,wq
tc,td,wh
td,wh,yn
ub,vc,wq'''),
    ('p1', 7)
])]

def parse(ip: str) -> Generator[tuple[str, str]]:
    for line in ip.splitlines():
        a, b = sorted(line.split('-'))
        yield a, b

def three_node_paths(ip: str) -> Generator[tuple[str, str, str]]:
    seen: defaultdict[str, set[str]] = defaultdict(set)

    for a, b in parse(ip):
        for c in seen[a]:
            c_, b_ = sorted((c, b))
            yield c_, a, b_
        
        for c in seen[b]:
            a_, c_ = sorted((a, c))
            yield a_, b, c_

        seen[a].add(b)
        seen[b].add(a)

def interconnected_groups(ip: str) -> Generator[frozenset[str, str, str]]:
    graph_as_set = set(parse(ip))

    for a, b, c in three_node_paths(ip):
        a_, c_ = sorted((a, c))

        if (a_, c_) in graph_as_set:
            yield frozenset({a, b, c})

def interconnected_groups_lines(ip: str) -> str:
    lines: list[str] = []
    groups = set(interconnected_groups(ip))
    sorted_groups = sorted(tuple(sorted(group)) for group in groups)

    for group in sorted_groups:
        lines.append(','.join(group))

    return '\n'.join(lines)    

def valid_interconnected_groups(ip: str) -> Generator[frozenset[str, str, str]]:
    for group in interconnected_groups(ip):
        a, b, c = list(group)

        if 't' in a[0] + b[0] + c[0]:
            yield group

def valid_interconnected_groups_lines(ip: str) -> str:
    lines: list[str] = []
    groups = set(valid_interconnected_groups(ip))
    sorted_groups = sorted(tuple(sorted(group)) for group in groups)

    for group in sorted_groups:
        lines.append(','.join(group))

    return '\n'.join(lines)    

def p1(ip: str) -> int:
    return len(set(valid_interconnected_groups(ip)))