from collections import deque
from typing import Iterator

test_inputs = [
	('example', 'mjqjpqmgbljsphdztnvjfqwrcgsmlb', [
		('p1', '7'), ('p2', '19')
	]),
	('example2', 'bvwbjplbgvbhsrlpgdmjqwftvncz', [
		('p1', 5), ('p2', '23')
	]),
	('example3', 'nppdvjthqldpwncqszvftbrmjlhg', [
		('p1', 6), ('p2', '23')
	]),
	('example4', 'nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg', [
		('p1', 10), ('p2', '29')
	]),
	('example5', 'zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw', [
		('p1', 11), ('p2', '26')
	])
]

def f(ip: str, length: int) -> int:
	queue = deque([], maxlen=length)

	for i, c in enumerate(ip):
		queue.append(c)

		if len(set(queue)) == length:
			return i + 1

def p1(ip: str) -> int:
	return f(ip, 4)

def p2(ip: str) -> int:
	return f(ip, 14)