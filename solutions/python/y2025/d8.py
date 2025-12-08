from collections import defaultdict
from collections.abc import Iterator
import itertools as it
from math import prod
from solutions.python.lib.unionfind import UnionFind

test_inputs = [('example', '''\
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689

10''', [
    ('p1', 40),
    ('p2', 25272),
])]

Point3D = tuple[int, int, int]

def distance_squared(p1: Point3D, p2: Point3D) -> int:
    return sum((p1[i] - p2[i])**2 for i in range(3))

def parse(ip: str) -> tuple[list[Point3D], int]:
    parts = ip.split('\n\n')
    lines: str

    match parts:
        case [lines]:
            count = 1000
        case [lines, count_string]:
            count = int(count_string)            

    boxes: list[Point3D] = []

    for line in lines.splitlines():
        print(line)
        x, y, z = map(int, line.split(','))
        boxes.append((x, y, z))

    return boxes, count

def p1(ip: str) -> int:
    boxes, count = parse(ip)
    
    pairs = sorted(
        it.combinations(boxes, 2),
        key=lambda p: distance_squared(*p)
    )

    uf = UnionFind(boxes)

    for box1, box2 in pairs[:count]:
        uf.union(box1, box2)

    circuits: defaultdict[Point3D, set[Point3D]] = defaultdict(set)

    for box in boxes:
        circuit_id = uf.find(box)
        circuits[circuit_id].add(box)

    circuit_lengths = sorted(len(circuit) for circuit in circuits.values())
    return prod(circuit_lengths[-3:])

def p2(ip: str) -> int:
    boxes, _ = parse(ip)
    
    pairs = sorted(
        it.combinations(boxes, 2),
        key=lambda p: distance_squared(*p)
    )

    uf = UnionFind(boxes)

    for box1, box2 in pairs:
        uf.union(box1, box2)

        circuit_ids = {uf.find(box) for box in boxes}

        if len(circuit_ids) == 1:
            return box1[0] * box2[0]
        
    assert False