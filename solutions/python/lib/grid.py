from abc import ABC, abstractmethod
from dataclasses import dataclass, field
import itertools as it
from typing import Callable, Generic, Iterable, Iterator, Self, Type, TypeVar

class Region(ABC):
	@abstractmethod
	def __contains__(self: Self, point: complex) -> bool:
		"""Returns True if the region contains the given point."""

@dataclass
class Rect(Region):
	"""A rectangular region in a grid.

	The bounds are inclusive, i.e. a Rect whose top and bottom are the same is considered to
	include a single row of points."""
	top: int
	right: int
	bottom: int
	left: int

	def __post_init__(self: Self) -> None:
		if self.top > self.bottom:
			raise ValueError(f'bottom ({bottom}) exceeds top ({top})')

		if self.left > self.right:
			raise ValueError(f'left ({left}) exceeds right ({right})')

	@property
	def width(self: Self) -> int:
		return self.right - self.left + 1

	@property
	def height(self: Self) -> int:
		return self.bottom - self.top + 1

	@property
	def top_left(self: Self) -> complex:
		return complex(self.left, self.top)

	@property
	def top_right(self: Self) -> complex:
		return complex(self.right, self.top)

	@property
	def bottom_right(self: Self) -> complex:
		return complex(self.right, self.bottom)

	@property
	def bottom_left(self: Self) -> complex:
		return complex(self.left, self.bottom)

	@classmethod
	def bounding(cls: Type[Self], points: Iterable[complex]) -> Self:
		iterator = iter(points)
		first = next(iterator)

		left = right = first.real
		top = bottom = first.imag

		for p in iterator:
			if p.real < left:
				left = p.real
			elif p.real > right:
				right = p.real

			if p.imag < top:
				top = p.imag
			elif p.imag > bottom:
				bottom = p.imag

		return cls(int(top), int(right), int(bottom), int(left))

	def __contains__(self: Self, point: complex) -> bool:
		return self.left <= point.real <= self.right and self.top <= point.imag <= self.bottom

	def __and__(self: Self, other: Self) -> Self:
		return self.__class__(max(self.top, other.top), min(self.right, other.right), min(self.bottom, other.bottom), max(self.left, other.left))

@dataclass
class Path(Region):
	"""A path in a grid made up of rows and columns of tiles."""

	points: list[complex]

	def __init__(self: Self, points: Iterable[complex]) -> None:
		self.points = list(points)

		if not self.points:
			raise ValueError('path cannot be empty')

		dupe_indices = []

		for i, (p, q) in enumerate(zip(self.points, self.points[1:])):
			if p == q:
				dupe_indices.append(i + 1)

			if p.real != q.real and p.imag != q.imag:
				raise ValueError(f'{i}th path segment ({p} to {q}) is diagonal')

		for i in dupe_indices[::-1]:
			del self.points[i]

	def __contains__(self: Self, point: complex) -> bool:
		return any(point in Rect.bounding((p, q)) for p, q in zip(self.points, self.points[1:]))

	def __iter__(self: Self) -> Iterator[complex]:
		yield self.points[0]

		for p, q in zip(self.points, self.points[1:]):
			d = q - p
			m = int(abs(d))
			assert m > 0
			u = d / m

			for _ in range(m):
				p += u
				yield p

T = TypeVar('T')

@dataclass
class Grid(Generic[T]):
	rows: list[list[T]]
	origin: complex

	def __init__(self: Self, rows: Iterable[Iterable[T]], origin: complex=0+0j) -> None:
		self.rows = [list(row) for row in rows]

		if not self.rows:
			raise ValueError('grid is empty')

		if not all(len(row) == len(self.rows[0]) for row in self.rows[1:]):
			raise ValueError('rows are not all same length')

		self.origin = origin

	@property
	def width(self) -> int:
		return len(self.rows[0])

	@property
	def height(self) -> int:
		return len(self.rows)

	def rect(self):
		return Rect.bounding((self.origin, self.origin + complex(self.width - 1, self.height - 1)))

	@classmethod
	def fromrect(cls: Type[Self], rect: Rect, label: Callable[[complex], T]) -> Self:
		return cls(
			(
				(label(complex(x, y)) for x in range(rect.left, rect.right + 1))
				for y in range(rect.top, rect.bottom + 1)
			),
			rect.top_left
		)

	def __getitem__(self, p: complex) -> T:
		return self.rows[int(p.imag - self.origin.imag)][int(p.real - self.origin.real)]

	def __setitem__(self, p: complex, v: T) -> None:
		self.rows[int(p.imag - self.origin.imag)][int(p.real - self.origin.real)] = v

@dataclass
class DefaultGrid(Generic[T]):
	"""An infinite grid whose values outside of a finite region are computed by a callback."""

	default: Callable[[complex], T]
	entries: dict[complex, T] = field(default_factory=lambda: {})

	def __getitem__(self, p: complex) -> T:
		try:
			return self.entries[p]
		except KeyError:
			return self.default(p)

	def __setitem__(self, p: complex, v: T) -> None:
		self.entries[p] = v

	def rect(self):
		return Rect.bounding(self.entries.keys())