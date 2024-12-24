from collections import defaultdict
from collections.abc import Generator
import itertools as it

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
    ('p1', 7),
    ('p2', 'co,de,ka,ta'),
])]

type Node = str

# An edge is a frozenset containing exactly two nodes.
type Edge = frozenset[Node]

def edge(a: Node, b: Node) -> Edge:
    return frozenset((a, b))

def parse(ip: str) -> Generator[Edge]:
    for line in ip.splitlines():
        yield edge(*line.split('-'))

# A three-node path is represented by a tuple (x, {a, b}) where x is the node
# in the middle of the path, and a and b are the endpoints of the path (their
# order is irrelevant).
type ThreeNodePath = tuple[Node, frozenset[Node]]

def three_node_path(a: Node, x: Node, b: Node) -> ThreeNodePath:
    return (x, frozenset((a, b)))

def three_node_paths(ip: str) -> Generator[ThreeNodePath]:
    seen: defaultdict[Node, set[Node]] = defaultdict(set)

    for a, b in parse(ip):
        for c in seen[a]:
            yield three_node_path(b, a, c)
        
        for c in seen[b]:
            yield three_node_path(a, b, c)

        seen[a].add(b)
        seen[b].add(a)

def interconnected_groups(ip: str) -> Generator[frozenset[Node]]:
    edges = set(parse(ip))

    for x, endpoints in three_node_paths(ip):
        a, b = endpoints

        if edge(a, b) in edges:
            yield frozenset((a, x, b))

def interconnected_groups_lines(ip: str) -> str:
    lines: list[str] = []
    groups = set(interconnected_groups(ip))
    sorted_groups = sorted(tuple(sorted(group)) for group in groups)

    for group in sorted_groups:
        lines.append(','.join(group))

    return '\n'.join(lines)    

def valid_interconnected_groups(ip: str) -> Generator[frozenset[Node]]:
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

def joinable(graph: set[Edge], g1: set[Node], g2: set[Node]) -> bool:
    # assume g1 and g2 are totally connected ((a, b) is an edge for every pair
    # of nodes a, b belonging to g1, same for g2)
    # we want to check whether g1 uu g2 is also totally connected
    # for that it suffices to check whether (a, b) is an edge for every pair of
    # nodes a, b where a in g1 and b in g2
    return all(edge(n1, n2) in graph for n1, n2 in it.product(g1, g2))

def p2(ip: str) -> int:
    edges = set(parse(ip))

    parent_dict: dict[Node, Node] = {}
    group_dict: dict[Node, set[Node]] = {}
    largest_group: set[Node] | None = None

    def add_singleton(node: str) -> str:
        nonlocal largest_group

        if node not in group_dict:
            group = {node}
            group_dict[node] = group

            if largest_group is None:
                largest_group = group

    def find(node: str) -> str:
        try:
            parent = parent_dict[node]
        except KeyError:
            return node
        else:
            result = find(parent)
            parent_dict[node] = result
            return result
        
    def union(n1: str, n2: str) -> str:
        nonlocal largest_group
        n1 = find(n1)
        n2 = find(n2)

        if n1 == n2:
            return
        
        g1 = group_dict[n1]
        g2 = group_dict[n2]

        if not joinable(edges, g1, g2):
            return

        s1 = len(g1)
        s2 = len(g2)

        if s1 < s2:
            n1, n2 = n2, n1

        parent_dict[n2] = n1
        n1_group = group_dict[n1]
        n1_group.update(group_dict[n2])

        if s1 + s2 > len(largest_group):
            largest_group = n1_group

    def add_pair(n1: str, n2: str) -> str:
        add_singleton(n1)
        add_singleton(n2)
        union(n1, n2)

    for n1, n2 in edges:
        add_singleton(n1)
        add_singleton(n2)
        union(n1, n2)

    return ','.join(sorted(largest_group))
