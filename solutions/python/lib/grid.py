from abc import ABC, abstractmethod
from dataclasses import dataclass, field
import itertools as it
from typing import Callable, Generic, Iterable, Iterator, Self, Type, TypeVar

class Region(ABC):
	@abstractmethod
	def __contains__(self: Self, point: complex) -> bool:
		"""Returns True if the region contains the given point."""

@dataclass
class Line(Region):
	"""A straight, un-directed line in the plane extending infinitely in both directions.

	The attributes `x_coeff`, `y_coeff` and `const_coeff` give an equation for the line:

	  ax + by + c = 0,

	where a = `x_coeff`, b = `y_coeff`, c = `const_coeff`. The coefficients are required to be 
	integers.
	"""

	x_coeff: int
	y_coeff: int
	const_coeff: int

	def __post_init__(self: Self) -> None:
		if not (self.a or self.b):
			raise ValueError('invalid line: both coefficients are zero')

	def coeffs(self) -> tuple[int, int, int]:
		return self.x_coeff, self.y_coeff, self.const_coeff

	def __contains__(self: Self, p: complex) -> bool:
		return self.x_coeff * p.real + self.y_coeff * p.imag == -self.const_coeff		

	def __and__(self: Self, other: Self) -> Self | complex | None:
		"""Returns the intersection of the two lines.

		If the two lines are the very same line, the result is that line. If the two lines are
		parallel, the result is `None`. Otherwise, the result is the unique point of intersection
		of the two lines.

		If the two lines are given by the equations

		  ax + by + c = 0,
		  Ax + By + C = 0,

		then we can find their intersection by solving those two equations simultaneously. This
		can be done using Gaussian elimination.

		First, we reorder the equations so that the leading coefficient of the first one is either
		in the same colum or to the left of the leading coefficient of the second one. That is, if
		a = 0 but A != 0, we swap the two equations. 

		Next, we need to make sure that the leading coefficient of the first equation is in fact
		to the left of the leading coefficient of the second equation. There are two cases to
		consider:

		- If the leading coefficient of the first equation is b, our system of equations is just

		    by + c = 0,
		    By + C = 0.

		  Adding the first equation scaled by -B/b to the second, and dividing the first equation
		  through by 1/b, we get

		     y + c/b      = 0,
		         C - Bc/b = 0.

		  If C - Bc/b turns out to be zero (i.e. bC = Bc), then the second equation is always
		  satisfied, so the solutions are all points (x, y) such that y = -c/b, and the two lines
		  must be the very same line. Otherwise, the second equation cannot be satisfied and hence
		  the lines do not intersect.

		- If the leading coefficient of the first equation is a, we add the first equation scaled
		  by -A/a to the second and divide the first equation through by a, giving

		    x + (b/a)     y + c/a      = 0,
		        (B - Ab/a)y + C - Ac/a = 0.

		  We can rewrite this as

		    x + (b/a)     y = -c/a,
		        (B - Ab/a)y = Ac/a - C.

          Then there are three possibilities to consider:

          - If B - Ab/a = 0 (i.e. aB = Ab) and Ac/a - C = 0 (i.e. Ac = aC), then any y satisfies
            the second equation, so the solutions are all points (x, y) such that
            x + (b/a) y = -c/a, and hence the two lines must be the very same line.

          - If B - Ab/a = 0 and Ac/a - C != 0, then the second equation cannot be satisfied, and
            hence the lines do not intersect.

          - If B - Ab/a != 0, we can divide the second equation through by B - Ab/a, giving

              x + (b/a)y = -c/a,
                       y = (Ac/a - C)/(B - Ab/a)
                         = (Ac - aC)/(aB - Ab).

            Hence

              x = -c/a - (b/a)y
                = -(c/a + (b/a)y)
                = -(c/a + (b/a)(Ac - aC)/(aB - Ab))
                = -(c/a + b(Ac - aC)/(a(aB - Ab)))
                = -(c(aB - Ab) + b(Ac - aC))/(a(aB - Ab))
                = -(aBc - Abc + Abc - abC)/(a(aB - Ab))
                = -(aBc - abC)/(a(aB - Ab))
                = -(Bc - bC)/(aB - Ab)
                = (bC - Bc)/(aB - Ab).
		"""
		a, b, c = self.coeffs()
		A, B, C = other.coeffs()

		if a == 0:
			if A != 0:
				return other & self

			if b * C == B * c:
				return self

			return None

		if a * B == A * b:
			if A * c == a * C:
				return self

			return None

		d = a * B - A * b
		return complex((b * C - B * c) / d, (A * c - C * a) / d)

	def __eq__(self: Self, other: Self) -> bool:
		"""Returns True if the two lines are the same as sets of points."""

		return a * B == A * b and A * c == a * C and (
			a != 0 or A != 0 or b * C == B * c
		)

		#return isinstance(self & other, self.__class__)

	def gradient(self: Self) -> float:
		return -self.x_coeff / self.y_coeff

	def intercept(self: Self) -> float:
		return -self.const_coeff / self.y_coeff
		
	@classmethod
	def fromeq(cls: Type[Self], m: float, c: float) -> Self:
		# y = mx + c
		# <==>
		# -mx + y - c = 0
		return cls(-m, 1, -c)

	@classmethod
	def from2points(cls: Type[Self], p0: complex, p1: complex) -> Self:
		# y = y0 + (y1 - y0)/(x1 - x0) * (x - x0)
		# <==>
		# -(y1 - y0)/(x1 - x0) x + y + x0 (y1 - y0)/(x1 - x0) - y0 = 0
		# <==>
		# (y0 - y1)x + (x1 - x0)y + x0 (y1 - y0) - y0 (x1 - x0) = 0

		x0, y0 = p0.real, p0.imag
		x1, y1 = p1.real, p1.imag
		dx = x1 - x0
		dy = y1 - y0
		return cls(-dy, dx, x0 * dy - y0 * dx)

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

	def __and__(self: Self, other: Self) -> Optional[Self]:
		try:
			return self.__class__(max(self.top, other.top), min(self.right, other.right), min(self.bottom, other.bottom), max(self.left, other.left))
		except ValueError:
			return None

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