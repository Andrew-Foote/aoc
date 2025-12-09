import itertools as it
import functools as ft
import operator
from typing import Iterable, Iterator

def common_value(first, *rest):
    assert all(i == first for i in rest)
    return first

def fiter(f, n, x):
    y = x

    for _ in range(n):
        y = f(y)

    return y

def sgn(x):
    if x == 0:
        return 0

def range_includes(bigrange: range, lilrange: range) -> bool:
	return bigrange.start <= lilrange.start and bigrange.stop >= lilrange.stop

def range_intersection(r1: range, r2: range) -> range:
	return range(max(r1.start, r2.start), min(r1.stop, r2.stop))

def prod(factors):
	return ft.reduce(operator.mul, factors, 1)

def interleave(it1: Iterable[int], it2: Iterable[int]) -> Iterator[int]:
	"""Interleave two infinite increasing sequences to form a new increasing sequence.
	
	>>> evens = (i*2 for i in it.count())
	>>> squares = (i**2 for i in it.count())
	>>> list(it.islice(interleave(evens, squares), 15))
	[0, 0, 1, 2, 4, 4, 6, 8, 9, 10, 12, 14, 16, 16, 18]
	"""
	itr1 = iter(it1)
	itr2 = iter(it2)

	nxt1 = next(itr1)
	nxt2 = next(itr2)

	while True:
		while nxt1 <= nxt2:
			yield nxt1
			nxt1 = next(itr1)

		while nxt2 < nxt1:
			yield nxt2
			nxt2 = next(itr2)

if __name__ == '__main__':
	import doctest
	doctest.testmod()