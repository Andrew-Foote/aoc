from collections import Counter
import itertools as it
from solutions.python.lib.numth import (
	primes_by_trial_division, primes, prime_factors, euclid, Cong,
	solve_coprime_congs, solve_congs
)

def test_primes_by_trial_division() -> None:
	ps = primes_by_trial_division()

	assert list(it.takewhile(lambda p: p < 100, ps)) == [
		2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
		67, 71, 73, 79, 83, 89, 97
	]

def test_primes() -> None:
	ps = primes()

	assert list(it.takewhile(lambda p: p < 100, ps)) == [
		2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
		67, 71, 73, 79, 83, 89, 97
	]

def test_prime_factors() -> None:
	assert prime_factors(1) == Counter()
	assert prime_factors(2) == Counter([2])
	assert prime_factors(6) == Counter([2, 3])
	assert prime_factors(9) == Counter([3, 3])
	assert prime_factors(100) == Counter([2, 2, 5, 5])
	assert prime_factors(169) == Counter([13, 13])
	
def test_euclid() -> None:
	assert euclid(240, 46) == (2, -9, 47, 23, -120)

def test_solve_coprime_congs() -> None:
	assert solve_coprime_congs([Cong(2, 3), Cong(3, 7)]) == Cong(17, 21)

def test_solve_congs() -> None:
	assert solve_congs([Cong(2, 2), Cong(6, 6)]) == Cong(0, 6)
