from collections import Counter, defaultdict
from dataclasses import dataclass, field
import functools as ft
import heapq
import itertools as it
import math
from typing import Iterator

# A simple prime generator using trial division.
def primes_by_trial_division() -> Iterator[int]:
	nums: Iterator[int] = it.count(2)

	while True:
		p = next(nums)
		yield p

		# I initially wrote `nums = (n for n in nums if n % p)`, but that
		# doesn't work since `p` is mutated with each iteration of the loop,
		# and the iterator will see the latest value of `p` when it
		# evaluates the condition.
		nums = (lambda p: (n for n in nums if n % p))(p)

# A prime generator using a form of the sieve of Eratosthenes, while still
# being able to generate primes indefinitely rather than up to a particular
# limit. This is adapted from the Haskell code on page 7 of:
#
# O'Neill, Melissa E. (2009). The genuine sieve of Eratosthenes. *Journal of
# Functional Programming*, 19(1), 95--106. Currently available online at
# https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf.
		
@dataclass(order=True)
class _Composite:
	"""A number that has been marked off as composite by the sieve of
	Eratosthenes, along with an iterator yielding an infinite sequence of
	larger numbers (in ascending order) that will also need to be marked
	off."""
	value: int
	successors: Iterator[int] = field(compare=False)

def primes() -> Iterator[int]:
	nums: Iterator[int] = it.count(2)
	heap: list[_Composite] = []

	while True:
		p = next(nums)
		next_composite = heap[0].value if heap else math.inf

		if next_composite <= p:
			while True:
				heapmin = heap[0]
				next_composite = heapmin.value

				if heapmin.value <= p:
					successors = heapmin.successors
					successor = next(successors)
					heapq.heappushpop(heap, _Composite(successor, successors))
				else:
					break
		else:
			yield p
			mult = p ** 2
			nums, nums_copy = it.tee(nums)
			successors = (lambda p: (n * p for n in nums_copy))(p)
			heapq.heappush(heap, _Composite(mult, successors))

def prime_factors(n: int) -> Counter[int]:
	result: Counter[int] = Counter()

	while n > 1:
		for p in primes():
			if not n % p:
				result[p] += 1
				n //= p
				break

	return result

@dataclass(frozen=True)
class Cong:
	rhs: int
	modulus: int

	def __post_init__(self) -> None:
		if self.modulus <= 0:
			raise ValueError(
				f'modulus is {self.modulus}, not a positive integer'
			)

	def __str__(self) -> str:
		return f'(x ≡ {self.rhs} mod {self.modulus})'

	@property
	def normed_rhs(self) -> int:
		return self.rhs % self.modulus

	def minsol(self, lb: int) -> int:
		"""Return the minimal solution that is greater than or equal to the
		specified lower bound."""

		a = self.rhs
		m = self.modulus

		# We want a + q * m where q is the smallest integer such that
		# a + q * m >= lb. This inequality can be rearranged to
		# q >= (lb - a) / m; hence the desired value of q is
		# math.ceil((lb - a) / m).

		return a + math.ceil((lb - a) / m) * m

	def maxsol(self, ub: int) -> int:
		"""Return the maximal solution that is less than or equal to the
		specified upper bound."""

		a = self.rhs
		m = self.modulus

		# We want a + q * m where q is the greatest integer such that
		# a + q * m <= ub. This inequality can be rearranged to
		# q <= (ub - a) / m; hence the desired value of q is
		# math.floor((ub - a) / m).

		return a + math.floor((ub - a) / m) * m

def display_congs(congs: list[Cong]) -> str:
	return '\n'.join(
		f'x ≡ {cong.rhs}  (mod {cong.modulus})' for cong in congs
	)

def euclid(a: int, b: int) -> tuple[int, int, int, int, int]:
	r0, r = a, b
	s0, s = 1, 0
	t0, t = 0, 1

	while r != 0:
		q = r0 // r
		r0, r = r, r0 - q * r
		s0, s = s, s0 - q * s
		t0, t = t, t0 - q * t

	return r0, s0, t0, s, t

def bezout_coeffs(a: int, b: int) -> tuple[int, int]:
	_, s, t, _, _ = euclid(a, b)
	return s, t

def _apply_crt(cong1: Cong, cong2: Cong) -> Cong:
	m1 = cong1.modulus
	m2 = cong2.modulus
	d, c1, c2, _, _ = euclid(m1, m2)
	assert d == 1, d
	s = cong1.rhs * c2 * m2 + cong2.rhs * c1 * m1
	m = m1 * m2
	result = Cong(s % m, m)
	return result

def solve_coprime_congs(congs: list[Cong]) -> Cong:
	"""Use the Chinese Remainder Theorem to solve a system of monic linear
	congruences with pairwise coprime moduli."""
	return ft.reduce(_apply_crt, congs, Cong(0, 1))

def solve_congs(congs: list[Cong]) -> Cong | None:
	"""Use the Chinese Remainder Theorem to solve a system of monic linear
	congruences. The moduli need not be pairwise coprime, although the solution
	will involve converting the system to one which does have pairwise coprime
	moduli, which is an expensive operation. If you already know that the
	moduli are pairwise coprime, it's better to use `solve_coprime_congs`."""

	# First, we replace each congruence x = a (mod m) with the system of
	# congruences x = a (mod p_1), ..., x = a (mod p_k), where p_1, ..., p_k
	# is the factorisation of m into prime powers. This is equivalent to the
	# original congruence by the Chinese Remainder Theorem, since p_1, ..., p_k
	# are pairwise coprime.

	# these are grouped by the prime factor of their modulus
	grouped_congs: defaultdict[int, list[Cong]] = defaultdict(list)

	for cong in congs:
		for p, n in prime_factors(cong.modulus).items():
			grouped_congs[p].append(Cong(cong.rhs, p ** n))

	# Now all congruences in the system have prime powers as moduli. So if any
	# two have non-coprime moduli, the moduli will be powers of the same prime.
	# Let's write the congruences as x = a (mod p^m) and x = b (mod p^n), where
	# m <= n. Then x = b (mod p^n) implies x = b (mod p^m). Hence if
	# a = b (mod p^m), we can drop x = a (mod p^m) from the system without
	# changing the solution set, while if a != b (mod p^m), the system is
	# unsolvable.

	new_congs: list[Cong] = []

	for p, congs in grouped_congs.items():
		min_modulus = min(cong.modulus for cong in congs)
		rhses = {cong.rhs % min_modulus for cong in congs}

		if len(rhses) == 1:
			new_congs.append(
				Cong(rhses.pop(), max(cong.modulus for cong in congs))
			)
		else:
			return None

	# Now we have a set of congruences with pairwise coprime moduli, so we can
	# apply the Chinese Remainder Theorem directly.

	return solve_coprime_congs(new_congs)