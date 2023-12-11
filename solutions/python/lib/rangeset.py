from bisect import bisect_right
from dataclasses import dataclass
from typing import Iterable, Iterator, Self
from solutions.python.lib.utils import range_intersection

class RangeSet:
	"""A disjoint union of contiguous ranges.

	Stored as a sorted list of "points", which are integers which are the start or end points of a
	range. Assigning indices to points according to the sorted order, points at even indices start
	a range, and points at odd indices end a range.
	"""

	points = list[int]

	def __init__(self: Self, points: Iterable[int]) -> None:
		self.points = sorted(points)

		if len(self.points) % 2 == 1:
			raise ValueError('range set has an odd number of points')

		if len(set(self.points)) != len(self.points):
			raise ValueError('duplicate points in range set')

	def __repr__(self: Self) -> str:
		return f'{self.__class__.__name__}({repr(self.points)})'

	@classmethod
	def from_ranges(cls: type[Self], ranges: Iterable[range]) -> Self:
		"""
		>>> RangeSet.from_ranges([range(1, 4), range(9, 16), range(3, 5)])
		RangeSet([1, 5, 9, 16])
		"""
		ranges = sorted(ranges, key=lambda r: (r.start, r.stop))
		points = []
		cur = None
		start = None
		stop = None

		for r in ranges:
			if cur is None:
				cur = r
			else:
				if r.start <= cur.stop:
					cur = range(cur.start, r.stop)
				else:
					points.extend([cur.start, cur.stop])
					cur = r

		if cur is not None:
			points.extend([cur.start, cur.stop])

		return cls(points)

	def __contains__(self: Self, x: int) -> bool:
		# Why does this work? bisect_right(self.points, x) returns i + 1, where i is the greatest
		# index i such that self.points[i] <= x, or 0 if there is no such index. So there are three
		# possibilities to consider:
		#
		# - If i = -1 (i.e. i + 1 = 0, which is even), then all of the ranges in the rangeset have
		# start points greater than x, so the range-set does not contain x.
		#
		# - If i = len(self.points) - 1 (i.e. i + 1 = len(self.points), which has to be even), then
		# self.points[i] is the end point of the last range in the rangeset, so the range-set does
		# not contain x.
		#
		# - Otherwise, x belongs to range(self.points[i], self.points[i + 1]), which means the
		# rangeset contains x if i is even (i + 1 is odd), and does not contain x if i is odd
		# (i + 1 is even).
		#
		# Overall, we see that the range-set contains x iff i + 1 is odd.

		return bisect_right(self.points, x) % 2 == 1

	def ranges(self: Self) -> Iterator[range]:
		"""
		>>> list(RangeSet([1, 4, 9, 16, 25, 36]).ranges())
		[range(1, 4), range(9, 16), range(25, 36)]
		"""
		for start, end in zip(self.points[::2], self.points[1::2]):
			yield range(start, end)

	def __iter__(self: Self) -> Iterator[int]:
		for r in self.ranges():
			yield from r

	def __and__(self: Self, other: Self) -> Self:
		"""
		>>> r1 = RangeSet([1, 4, 9, 16, 25, 36])
		>>> r2 = RangeSet([0, 2, 3, 18])
		>>> r1 & r2
		RangeSet([1, 2, 3, 4, 9, 16])
		"""
		ranges = []

		for self_range in self.ranges():
			for other_range in other.ranges():
				intersection = range_intersection(self_range, other_range)

				if intersection:
					ranges.append(intersection)

		return self.__class__.from_ranges(ranges)

if __name__ == '__main__':
	import doctest
	doctest.testmod()