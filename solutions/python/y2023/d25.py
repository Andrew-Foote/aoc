from collections import defaultdict
import itertools as it

test_inputs = [
	('example', '''\
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr''', [
		('p1', 54)
	])
]

def parse(ip: str) -> set[tuple[str, str]]:
	g = set()

	for line in ip.splitlines():
		x, ys = line.split(':')

		for y in ys.split():
			g.add((x, y))

	return g

def graph_as_dict(g: set[tuple[str, str]]):
	d = defaultdict(lambda: set())

	for e1, e2 in g:
		d[e1].add(e2)
		d[e2].add(e1)

	return d

def connected_set(g: set[tuple[str, str]], n: str) -> set[str]:
	d = graph_as_dict(g)
	es = {n}
	visited = set()

	while es:
		new_es = set()

		for e in es:
			visited.add(e)
			new_es.update(d[e] - visited)

		es = new_es

	return visited

def gnodes(g: set[tuple[str, str]]) -> set[str]:
	return {e1 for e1, e2 in g} | {e2 for e1, e2 in g}

def connected_components(g: set[tuple[str, str]]) -> list[set[str]]:
	components = []
	not_visited = gnodes(g)

	while not_visited:
		e = not_visited.pop()
		cs = connected_set(g, e)
		components.append(cs)

		for ce in cs:
			if ce in not_visited:
				not_visited.remove(ce)

	return components

def sortnodesbydegreee(g: set[tuple[str, str]]) -> list[str]:
	d = graph_as_dict(g)
	return sorted(d.keys(), key=lambda k: len(d[k]))

def sortedgesbydegree(g: set[tuple[str, str]]) -> list[str]:
	d = graph_as_dict(g)
	return sorted(g, key=lambda t: -(len(d[t[0]]) + len(d[t[1]])))

def p1(ip: str) -> int:
	g = parse(ip)
	# we need to remove three edges from the graph to divide it into
	# two connected components

	# print(list(g))
	# print(sortedgesbydegree(g))
	# input()

	for e1, e2, e3 in it.combinations(sortedgesbydegree(g), 3):
		# if an edge has degree 1
		# then removing it can't disconnect the graph

		gmod = g - {e1, e2, e3}
		comps = connected_components(gmod)
		print(e1, e2, e3, '/', comps)

		if len(comps) == 2:
			return len(comps[0]) * len(comps[1])

# we can expand the map into a dictionary mapping each edge
# to the set of all paths from one edge to another