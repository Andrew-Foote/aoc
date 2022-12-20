from collections.abc import Iterable
from dataclasses import dataclass
import numpy as np
import scipy.optimize
from typing import Self

@dataclass(frozen=True)
class LinPoly:
	"""A linear combination of variables plus a constant term."""
	var_coeffs: dict[str, int]
	const_coeff: int=0

	def __add__(self: Self, other: Self | int) -> Self:
		if isinstance(other, int):
			return self + const(other)

		new_coeffs = {}

		for x in free_vars(self) | free_vars(other):
			new_coeff = self.var_coeffs.get(x, 0) + other.var_coeffs.get(x, 0)

			if new_coeff:
				new_coeffs[x] = new_coeff

		return self.__class__(new_coeffs, self.const_coeff + other.const_coeff)

	def __radd__(self: Self, other: Self | int) -> Self:
		if isinstance(other, int):
			return const(other) + self

		new_coeffs = {}

		for x in free_vars(other) | free_vars(self):
			new_coeff = other.var_coeffs.get(x, 0) + self.var_coeffs.get(x, 0)

			if new_coeff:
				new_coeffs[x] = new_coeff

		return self.__class__(new_coeffs, other.const_coeff + self.const_coeff)

	def __mul__(self: Self, other: int) -> Self:
		if other == 0:
			return self.__class__({})

		return self.__class__(
			{x: a * other for x, a in self.var_coeffs.items()},
			self.const_coeff * other
		)

	def __rmul__(self: Self, other: int) -> Self:
		if other == 0:
			return self.__class__({})

		return self.__class__(
			{x: other * a for x, a in self.var_coeffs.items()},
			other * self.const_coeff
		)

	def __neg__(self: Self):
		return self * (-1)

	def __sub__(self: Self, other: Self | int) -> Self:
		return self + (-other)

	def __rsub__(self: Self, other: Self | int) -> Self:
		return (-other) + self

	def __str__(self: Self) -> str:
		terms = sorted(((a, x) for x, a in self.var_coeffs.items()), key=lambda t: t[1])
	
		if self.const_coeff:
			terms.append((self.const_coeff, 1))

		for i, (a, x) in enumerate(terms):
			if a >= 0:
				if i != 0:
					terms[0], terms[1:i + 1] = terms[i], terms[:i]
				negate = False
				break
		else:
			negate = True
			terms = [(-a, x) for a, x in terms]

		if not terms:
			return '0'

		term_s = []

		for a, x in terms:
			a = abs(a)

			if a == 1:
				term_s.append(str(x))
			elif x == 1:
				term_s.append(str(a))
			else:
				term_s.append(f'{a} * {x}')

		signs = [''] + [(' + ' if a >= 0 else ' - ') for a, x in terms[1:]]
		term_s = ''.join(sign + t for sign, t in zip(signs, term_s))

		if negate:
			if len(terms) > 1:
				term_s = f'({term_s})'

			term_s = f'-{term_s}'

		return term_s

def var(name: str) -> LinPoly:
	return LinPoly({name: 1})

def const(value: int) -> LinPoly:
	return LinPoly({}, value)

@dataclass(frozen=True)
class Equation:
	lhs: LinPoly
	rhs: LinPoly

	def normalize(self: Self) -> Self:
		lhs_vars = self.lhs - self.lhs.const_coeff
		rhs_vars = self.rhs - self.rhs.const_coeff
		return self.__class__(lhs_vars - rhs_vars, const(self.rhs.const_coeff - self.lhs.const_coeff))

	def __str__(self: Self) -> str:
		return f'{self.lhs} = {self.rhs}'

def eq(lhs: LinPoly | int, rhs: LinPoly | int) -> Equation:
	if isinstance(lhs, int): lhs = const(lhs)
	if isinstance(rhs, int): rhs = const(rhs)
	return Equation(lhs, rhs)

@dataclass(frozen=True)
class LeInequality:
	lhs: LinPoly
	rhs: LinPoly

	def normalize(self: Self) -> Self:
		lhs_vars = self.lhs - self.lhs.const_coeff
		rhs_vars = self.rhs - self.rhs.const_coeff
		return self.__class__(lhs_vars - rhs_vars, const(self.rhs.const_coeff - self.lhs.const_coeff))

	def __str__(self: Self) -> str:
		return f'{self.lhs} <= {self.rhs}'

def le(lhs: LinPoly | int, rhs: LinPoly | int) -> LeInequality:
	if isinstance(lhs, int): lhs = const(lhs)
	if isinstance(rhs, int): rhs = const(rhs)
	return LeInequality(lhs, rhs)

@dataclass(frozen=True)
class GeInequality:
	lhs: LinPoly
	rhs: LinPoly

	def normalize(self: Self) -> Self:
		lhs_vars = self.lhs - self.lhs.const_coeff
		rhs_vars = self.rhs - self.rhs.const_coeff
		return self.__class__(lhs_vars - rhs_vars, const(self.rhs.const_coeff - self.lhs.const_coeff))

	def __str__(self: Self) -> str:
		return f'{self.lhs} >= {self.rhs}'

def ge(lhs: LinPoly | int, rhs: LinPoly | int) -> GeInequality:
	if isinstance(lhs, int): lhs = const(lhs)
	if isinstance(rhs, int): rhs = const(rhs)
	return GeInequality(lhs, rhs)

Constraint = Equation | LeInequality | GeInequality

def free_vars(e: LinPoly | Constraint) -> set[str]:
	if isinstance(e, LinPoly):
		return set(e.var_coeffs.keys())
	else:
		return free_vars(e.lhs) | free_vars(e.rhs)

def solve(objective: LinPoly, constraints: Iterable[Constraint]):
	constraints = [constraint.normalize() for constraint in constraints]
	xs = sorted(free_vars(objective).union(*(free_vars(constraint) for constraint in constraints)))
	# don't forget to handle the objective const_coeff
	objective_vec = [objective.var_coeffs.get(x, 0) for x in xs]
	integrality = [1 for x in xs]
	lbs = [-np.inf for x in xs]
	ubs = [np.inf for x in xs]
	constraint_rows = []
	constraint_lbs = []
	constraint_ubs = []

	for constraint in constraints:
		a = constraint.rhs.const_coeff

		if len(constraint.lhs.var_coeffs) == 1:
			x = list(constraint.lhs.var_coeffs)[0]
			i = xs.index(x)
			lb = lbs[i]
			ub = ubs[i]

			if isinstance(constraint, Equation):
				lbs[i] = ubs[i] = a
			elif isinstance(constraint, LeInequality):
				lbs[i] = lb
				ubs[i] = a
			elif isinstance(constraint, GeInequality):
				lbs[i] = a
				ubs[i] = ub
			else:
				assert False
		else:
			constraint_rows.append([constraint.lhs.var_coeffs.get(x, 0) for x in xs])

			if isinstance(constraint, Equation):
				lb = ub = a
			elif isinstance(constraint, LeInequality):
				lb, ub = -np.inf, a
			elif isinstance(constraint, GeInequality):
				lb, ub = a, np.inf
			else:
				assert False

			constraint_lbs.append(lb)
			constraint_ubs.append(ub)

	bounds = scipy.optimize.Bounds(lbs, ubs)
	constraint = scipy.optimize.LinearConstraint(np.array(constraint_rows), constraint_lbs, constraint_ubs)
	# print()
	# print(objective_vec)
	# print(len(objective_vec))
	# print(bounds)
	# print(constraint)
	# print()
	result = scipy.optimize.milp(objective_vec, integrality=integrality, bounds=bounds, constraints=constraint, options={'disp': True})

	if not result.success:
		raise ValueError(f'Optimal solution not found.\n{result}')

	# print(result)

	#breakpoint()

	return result.x, result.fun + objective.const_coeff


