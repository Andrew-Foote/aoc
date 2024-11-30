from abc import ABC
from dataclasses import dataclass
import string
from typing import assert_never

@dataclass(frozen=True)
class Lit[T]:
	val: T

	def __and__(self, other: 'Prop[T]') -> 'Prop[T]':
		return And(self, other)

	def __or__(self, other: 'Prop[T]') -> 'Prop[T]':
		return Or(self, other)

@dataclass(frozen=True)
class And[T]:
	lhs: 'Prop[T]'
	rhs: 'Prop[T]'

	def __and__(self, other: 'Prop[T]') -> 'Prop[T]':
		return And(self, other)

	def __or__(self, other: 'Prop[T]') -> 'Prop[T]':
		return Or(self, other)

@dataclass(frozen=True)
class Or[T]:
	lhs: 'Prop[T]'
	rhs: 'Prop[T]'

	def __and__(self, other: 'Prop[T]') -> 'Prop[T]':
		return And(self, other)

	def __or__(self, other: 'Prop[T]') -> 'Prop[T]':
		return Or(self, other)

type Prop[T] = Lit[T] | And[T] | Or[T]

def dnf(prop: Prop) -> list[list[Lit]]:
	match prop:
		case Lit():
			return [[prop]]
		case And(lhs, rhs):
			ldnf = dnf(lhs)
			rdnf = dnf(rhs)
			result: list[list[Lit]] = []

			for lclause in ldnf:
				lset = set(lclause)

				for rclause in rdnf:
					rclause = [lit for lit in rclause if lit not in lset]
					result.append(lclause + rclause)

			return result
		case Or(lhs, rhs):
			return dnf(lhs) + dnf(rhs)
		case _:
			assert_never(prop)

def alphalits(count: int=26) -> list[Lit[str]]:
	return [Lit(letter) for letter in string.ascii_lowercase[:count]]

def test_dnf():
	a, b, c, e, f, g, h, i, j, *_ = alphalits()
	
	assert dnf(
		((a & b) | c)
		& ((e & f & g) | h | (i & j))
	) == [
		[a, b, e, f, g], [a, b, h], [a, b, i, j],
		[c, e, f, g], [c, h], [c, i, j]
	]