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

	def __str__(self) -> str:
		return str(self.val)

@dataclass(frozen=True)
class TrueProp[T]:
	def __and__(self, other: 'Prop[T]') -> 'Prop[T]':
		return And(self, other)

	def __or__(self, other: 'Prop[T]') -> 'Prop[T]':
		return Or(self, other)	

	def __str__(self) -> str:
		return '⊤'

@dataclass(frozen=True)
class FalseProp[T]:
	def __and__(self, other: 'Prop[T]') -> 'Prop[T]':
		return And(self, other)

	def __or__(self, other: 'Prop[T]') -> 'Prop[T]':
		return Or(self, other)

	def __str__(self) -> str:
		return '⊥'

@dataclass(frozen=True)
class And[T]:
	lhs: 'Prop[T]'
	rhs: 'Prop[T]'

	def __and__(self, other: 'Prop[T]') -> 'Prop[T]':
		return And(self, other)

	def __or__(self, other: 'Prop[T]') -> 'Prop[T]':
		return Or(self, other)

	def __str__(self) -> str:
		return f'({self.lhs} & {self.rhs})'

@dataclass(frozen=True)
class Or[T]:
	lhs: 'Prop[T]'
	rhs: 'Prop[T]'

	def __and__(self, other: 'Prop[T]') -> 'Prop[T]':
		return And(self, other)

	def __or__(self, other: 'Prop[T]') -> 'Prop[T]':
		return Or(self, other)

	def __str__(self) -> str:
		return f'({self.lhs} | {self.rhs})'

type Prop[T] = Lit[T] | TrueProp[T] | FalseProp[T] | And[T] | Or[T]

def dnf(prop: Prop) -> list[list[Lit]]:
	match prop:
		case Lit():
			return [[prop]]
		case TrueProp():
			return [[]]
		case FalseProp():
			return []
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
