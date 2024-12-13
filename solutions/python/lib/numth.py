from collections import Counter, defaultdict
from dataclasses import dataclass, field
from fractions import Fraction
import functools as ft
import heapq
import itertools as it
import math
from typing import Iterator

def primes_by_trial_division() -> Iterator[int]:
	"""A simple prime generator using trial division."""
	nums: Iterator[int] = it.count(2)

	while True:
		p = next(nums)
		yield p

		# I initially wrote `nums = (n for n in nums if n % p)`, but that
		# doesn't work since `p` is mutated with each iteration of the loop,
		# and the iterator will see the latest value of `p` when it
		# evaluates the condition.
		nums = (lambda p: (n for n in nums if n % p))(p)
		
@dataclass(order=True)
class _Composite:
	"""A number that has been marked off as composite by the sieve of
	Eratosthenes, along with an iterator yielding an infinite sequence of
	larger numbers (in ascending order) that will also need to be marked
	off."""
	value: int
	successors: Iterator[int] = field(compare=False)

def primes() -> Iterator[int]:
	"""A prime generator using a form of the sieve of Eratosthenes, while still
	being able to generate primes indefinitely rather than up to a particular
	limit. This is adapted from the Haskell code on page 7 of:

	O'Neill, Melissa E. (2009). The genuine sieve of Eratosthenes. *Journal of
	Functional Programming*, 19(1), 95--106. Currently available online at
	https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf."""

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
	"""Return a `Counter` containing the prime factors of the given integer,
	with their multiplicities as counts."""

	result: Counter[int] = Counter()

	while n > 1:
		for p in primes():
			if not n % p:
				result[p] += 1
				n //= p
				break

	return result

def eea(a: int, b: int) -> tuple[int, int, int, int, int]:
	"""Run the extended Euclidean algorithm on a and b and return
	(d, r, s, a', b') where d = gcd(a, b), r and s are integers such that
	ra + sb = d, a' = a /// d and b' = b // d."""

	r0, r = a, b
	s0, s = 1, 0
	t0, t = 0, 1

	while r != 0:
		q = r0 // r
		r0, r = r, r0 - q * r
		s0, s = s, s0 - q * s
		t0, t = t, t0 - q * t

	return r0, s0, t0, abs(t), abs(s)

def bezout_coeffs(a: int, b: int) -> tuple[int, int]:
	"""Return integers (r, s) such that ra + sb = gcd(a, b)."""

	_, s, t, _, _ = eea(a, b)
	return s, t

@dataclass(frozen=True)
class ResidueClass:
	"""The set of all integers congruent to `residue` modulo `modulus`."""

	residue: int
	modulus: int

	def __post_init__(self) -> None:
		if self.modulus == 0:
			raise ValueError('zero modulus')

	def cong_str(self) -> str:
		return f'(x ≡ {self.residue} mod {self.modulus})'

	@property
	def norm_residue(self) -> int:
		return self.residue % self.modulus

	def __contains__(self, x: int) -> bool:
		return x % self.modulus == self.residue

	def minsol(self, lb: int) -> int:
		"""Return the minimal element that is greater than or equal to the
		specified lower bound."""

		a = self.residue
		m = self.modulus

		# We want a + q * m where q is the smallest integer such that
		# a + q * m >= lb. This inequality can be rearranged to
		# q >= (lb - a) / m; hence the desired value of q is
		# math.ceil((lb - a) / m).

		return a + math.ceil((lb - a) / m) * m

	def maxsol(self, ub: int) -> int:
		"""Return the maximal solution that is less than or equal to the
		specified upper bound."""

		a = self.residue
		m = self.modulus

		# We want a + q * m where q is the greatest integer such that
		# a + q * m <= ub. This inequality can be rearranged to
		# q <= (ub - a) / m; hence the desired value of q is
		# math.floor((ub - a) / m).

		return a + math.floor((ub - a) / m) * m
	
	def bound(self, start: int, stop: int) -> range:
		"""Return a range consisting of all solutions within the given
		bounds."""

		return range(self.minsol(start), stop, self.modulus)

def solve_lincong(a: int, b: int, m: int) -> ResidueClass | None:
	"""Solve the linear congruence ax = b (mod m), returning either a
	`ResidueClass` object representing the set of all solutions, or `None` if
	there are no solutions."""

	# If a is coprime to m, then by using the extended Euclidean algorithm we
	# can find integers r and s such that ra + sm = 1, i.e. ra = (-s)m + 1;
	# hence ra = 1 modulo m, meaning r is an inverse of a modulo m. So then
	# ax = b (mod m) is equivalent to x = rb (mod m), and so rb is the unique
	# solution.
	#
	# Otherwise, the extended Euclidean algorithm can still be used to compute
	# d = gcd(a, m). If d != 0 it will also give us a' = a / d, m' = m / d.
	# In the exceptional case where d = 0, we have a = m = 0, and "modulo 0"
	# is generally considered to be undefined; however we can regard 0 = b
	# (mod 0) as equivalent to the condition that 0 = q0 + b for some integer q,
	# i.e. 0 = b. So in this case, we can say that the solution set is the set
	# of all integers if b = 0, and the empty set otherwise.
	#
	# Now, ax = b (mod m) holds iff ax = qm + b, for some integer q, and given
	# a' = a / d and m' = m / d, we can rewrite this equation as
	# a'dx = qm'd + b, or equivalently (a'x - qm')d = b. From this we see that
	# if b is not divisible by d, there will be no solutions. Otherwise, we may
	# take b' = b / d, and then we see that ax = qm + b is equivalent to
	# a'x = qm' + b' (from dividing both sides by d). So the congruence
	# ax = b (mod m) is equivalent in this case to a'x = b' (mod m'). And a'
	# and m' are guaranteed to be coprime, so we can solve this equation in the
	# way described above. In fact, from our first use of the extended Euclidean
	# algorithm we will already have integers r and s such that ra + sm = d,
	# and hence ra' + sm' = 1, so rb' is the unique solution modulo m'. (Note
	# that this is modulo m', not modulo m; there will be multiple solutions
	# modulo m, namely all integers modulo m of the form rb' + qm' where q is
	# an integer.)

	d, r, _, _, m_div_d = eea(a, m)

	if d == 0:
		if b == 0:
			return ResidueClass(0, 1)

		return None

	b_div_d, b_mod_d = divmod(b, d)

	if b_mod_d:
		return None
			
	return ResidueClass(r * b_div_d, m_div_d)

def _apply_crt(cong1: ResidueClass, cong2: ResidueClass) -> ResidueClass:
	m1 = cong1.modulus
	m2 = cong2.modulus
	d, c1, c2, _, _ = eea(m1, m2)
	assert d == 1, d
	s = cong1.residue * c2 * m2 + cong2.residue * c1 * m1
	m = m1 * m2
	result = ResidueClass(s % m, m)
	return result

def solve_coprime_congs(congs: list[ResidueClass]) -> ResidueClass:
	"""Use the Chinese Remainder Theorem to solve a system of monic linear
	congruences with pairwise coprime moduli."""
	return ft.reduce(_apply_crt, congs, ResidueClass(0, 1))

def solve_congs(congs: list[ResidueClass]) -> ResidueClass | None:
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
	grouped_congs: defaultdict[int, list[ResidueClass]] = defaultdict(list)

	for cong in congs:
		for p, n in prime_factors(cong.modulus).items():
			grouped_congs[p].append(ResidueClass(cong.rhs, p ** n))

	# Now all congruences in the system have prime powers as moduli. So if any
	# two have non-coprime moduli, the moduli will be powers of the same prime.
	# Let's write the congruences as x = a (mod p^m) and x = b (mod p^n), where
	# m <= n. Then x = b (mod p^n) implies x = b (mod p^m). Hence if
	# a = b (mod p^m), we can drop x = a (mod p^m) from the system without
	# changing the solution set, while if a != b (mod p^m), the system is
	# unsolvable.

	new_congs: list[ResidueClass] = []

	for p, congs in grouped_congs.items():
		min_modulus = min(cong.modulus for cong in congs)
		rhses = {cong.rhs % min_modulus for cong in congs}

		if len(rhses) == 1:
			new_congs.append(
				ResidueClass(rhses.pop(), max(cong.modulus for cong in congs))
			)
		else:
			return None

	# Now we have a set of congruences with pairwise coprime moduli, so we can
	# apply the Chinese Remainder Theorem directly.

	return solve_coprime_congs(new_congs)

def line_intersection(
	coeffs1: tuple[int, int, int], coeffs2: tuple[int, int, int]
) -> tuple[Fraction, Fraction] | None:
	
	a, b, c = coeffs1
	A, B, C = coeffs2

	if a == 0:
		if A != 0:
			return line_intersection(coeffs2, coeffs1)

		if b * C == B * c:
			return None # lines are identical, any point on them is a solution

		return None # no solutions

	if a * B == A * b:
		if A * c == a * C:
			return None # lines are identical, any point on them is a solution

		return None # no solutions

	d = a * B - A * b
	return Fraction(B * c - b * C, d), Fraction(a * C - A * c, d)