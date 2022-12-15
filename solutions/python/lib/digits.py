import functools as ft
import math
from typing import overload, TypeVar

@ft.lru_cache(10)
def log_of_2(b):
	return math.log(2, b)

LOG10_2 = math.log10(2)

def digit_count(n: int, b: int=10) -> int:
	"""Count the digits in n's base-b expansion.

	Based on this StackOverflow answer: https://stackoverflow.com/a/74099536/19822079

	We start with the bit count, which is equal to [log_2 n] + 1 (square brackets denote flooring),
	so subtracting 1 gives us [log_2 n], i.e. the unique integer k such that

      log_2 n - 1 <= k < log_2 n.                                              (1)

	From the change-of-base formula for logarithms, we have

	  log_2 n = log_b n / log_b 2

	and hence

	  log_b n = log_2 n * log_b 2.

	Multiplying (1) through by log_b 2 therefore gives us

	  log_b n - log_b 2 < k log_b 2 <= log_b n.

	Given that b > 1, we have log_b 2 <= 1, hence log_b n - 1 <= log_b n - log_b 2, so

	  log_b n - 1 < k log_b 2 <= log_b n,

	hence

	  [log_b n] - 1 = [log_b n - 1] <= [k log_b 2] <= [log_b n].

	So [k log_b 2] could be either [log_b n] or [log_b n] - 1. To check for the latter possibility
	we can calculate b^[k log_b 2]. Since

	  log_b n - 1 < [log_b n] <= log_b n and log_b n - 2 < [log_b n] - 1 <= log_b n - 1,

	we have

	  n / b < b^[log_b n] <= n and n / b^2 < b^([log_b n] - 1) <= n / b,

	so we have [k log_b 2] = [log_b n] - 1 iff b^[k log_b 2] <= n / b, i.e. b^[k log_b 2 + 1] <= n.
	"""

	k = n.bit_length() - 1
	e = int(k * log_of_2(b)) + 1
	return e + (b ** e <= n)

_T = TypeVar('_T')

class digits:
	__slots__ = ('n', 'b')

	n: int
	b: int

	def __init__(self, n: int, b: int=10):
		self.n = n
		self.b = b

	def __repr__(self) -> str:
		return f'digits({self.n}, {self.b})'

	def __eq__(self, other) -> bool:
		return isinstance(other, digits) and self.n == other.n and self.b == other.b

	def __len__(self) -> int:
		return digit_count(self.n, self.b)

	def __iter__(self):
		n = self.n
		b = self.b

		while n:
			n, r = divmod(n, b)
			yield r

	def __reversed__(self):
		for i in range(len(self) - 1, -1, -1):
			yield self[i]

	def __getitem__(self, place: int) -> int:
		if not 0 <= place < len(self):
			raise IndexError(f'no digit at place {place}')

		b = self.b
		return (self.n % b ** (place + 1)) // b ** place

	@overload
	def get(self, place: int, default: int=0) -> int:
		...

	@overload
	def get(self, place: int, default: _T) -> int | _T:
		...

	def get(self, place, default):
		try:
			return self[place]
		except IndexError:
			return default

	def __setitem__(self, place: int, d: int) -> None:
		b = self.b

		if not 0 <= d < b:
			raise ValueError(f'{d} is not an allowed digit for base {b}')

		self.n += (d - self[place]) * b ** place

def digit(n: int, place: int, b: int=10) -> int:
	return digits(n, b)[place]