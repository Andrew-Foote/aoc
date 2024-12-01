from collections import Counter

test_inputs = [
	('example', '''\
3   4
4   3
2   5
1   3
3   9
3   3''', [
		('p1', 11),
		('p2', 31),
	])
]

def parse(ip) -> tuple[list[int], list[int]]:
	lines = ip.splitlines()
	list1 = []
	list2 = []

	for line in lines:
		i1, i2 = line.split()
		list1.append(int(i1.strip()))
		list2.append(int(i2.strip()))

	return list1, list2

def p1(ip) -> int:
	list1, list2 = parse(ip)
	list1.sort()
	list2.sort()
	return sum(abs(i1 - i2) for i1, i2 in zip(list1, list2))

def p2(ip) -> int:
	list1, list2 = parse(ip)
	counter = Counter(list2)
	return sum(i * counter[i] for i in list1)