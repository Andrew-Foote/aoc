from bisect import bisect_right
from dataclasses import dataclass
from typing import Iterator

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

	def __iter__(self: Self) -> Iterator[int]:
		for start, end in zip(self.points, self.points[1:]):
			yield from range(start, end)

	# def add(self, r: range) -> None:
	# 	if r.step != 1:
	# 		raise ValueError('ranges in rangeset must be contiguous')

	# 	# Find the greatest index i such that self.points[i] <= r.start.

	# 	# p[i] <= r.start < p[i + 1]
	# 	i = bisect_right(self.points, r.start) - 1

	# 	# If i == -1, then the start point of the range set is greater than r.start.
	# 	# So we will be lowering the start point. But will we be adding a new range in front
	# 	# or just extending the first range? We need to check whether r.end is strictly less
	# 	# than the first point. If it's strictly less there's a gap of at least 1, otherwise
	# 	# we can merge.

	# 	# Find the smallest index i such that r.end < self.points[i].
	# 	# p[j - 1] <= r.end < p[j]
	# 	j = bisect_right(self.points, r.end)

	# 	# Find the smallest index i such that self.points[i] > 