import cmath
from collections.abc import Iterable
from dataclasses import dataclass
import itertools as it
import math
import operator
from typing import overload, Self, TypeVar

# A rational function consists of a polynomial numerator and a polynomial denominator.
# A (univariate) polynomial consists of a variable and a sequence of coefficients, which are constants
# (i.e. expressions in which the aforementioned variable does not occur).
# 

_T = TypeVar('_T')

@dataclass
class Polynomial:
    coeffs: list[int] # sorted by degree

    def __init__(self: Self, coeffs: Iterable[int] | int=()) -> None:
        if isinstance(coeffs, int):
            self.coeffs = [coeffs]
        else:
            self.coeffs = []
            len_ = None

            for i, coeff in enumerate(coeffs):
                if coeff != 0:
                    len_ = i + 1

                self.coeffs.append(coeff)

            del self.coeffs[len_:]

    def __len__(self: Self) -> int:
        return len(self.coeffs)

    def __bool__(self: Self) -> bool:
        return bool(self.coeffs)

    @overload
    def degree(self: Self, ifzero: float=-math.inf) -> float:
        ...

    @overload
    def degree(self: Self, ifzero: _T) -> int | _T:
        ...

    def degree(self, ifzero=-math.inf):
        if not self:
            return ifzero
        else:
            return len(self) - 1

    def __getitem__(self: Self, degree: int) -> int:
        return 0 if degree > self.degree() else self.coeffs[degree]

    def __call__(self: Self, x: complex) -> complex:
        result: complex = 0

        for coeff in reversed(self.coeffs):
            result *= x
            result += coeff

        return result

    def __str__(self: Self) -> str:
        if not self: return '0'
        term_strings = []
        n = self.degree(0)

        for i, coeff in enumerate(reversed(self.coeffs)):
            if coeff == 0: continue
            leading = i == 0

            sign = {
                (True, True): '',
                (True, False): ' + ',
                (False, True): '-',
                (False, False): ' - '
            }[coeff >= 0, leading]

            if i == n:
                term_strings.append(sign + str(abs(coeff)))
            else:
                coeff_string = '' if abs(coeff) == 1 else f'{abs(coeff)} * '
                x_string = 'x' if i == n - 1 else f'x ** {n - i}'
                term_strings.append(sign + coeff_string + x_string)

        return ''.join(term_strings)

    def __add__(self: Self, other: Self | int) -> Self:
        if isinstance(other, int):
            return self + self.__class__(other)

        return self.__class__(a + b for a, b in it.zip_longest(self.coeffs, other.coeffs, fillvalue=0))

    def __radd__(self: Self, other: Self | int) -> Self:
        return self + other

    def __mul__(self: Self, other: Self | int) -> Self:
        # (sum_(k = 0)^m a_k x^k)(sum_(k = 0)^n b_k x^k)
        # = sum_(i = 0)^m sum_(j = 0)^n a_i b_j x^(i + j)
        # = sum_(i = 0)^(m + n) (sum_(j = 0)^i a_j b^(i - j)) x^k
        if isinstance(other, int):
            return self * self.__class__(other)

        return self.__class__(
            sum(self[j] * other[i - j] for j in range(i + 1))
            for i in range(len(self) + len(other))
        )

    def __rmul__(self: Self, other: Self | int) -> Self:
        return self * other

    def __neg__(self: Self) -> Self:
        return self * -1

    def __sub__(self: Self, other: Self | int) -> Self:
        return self + (-other)

    def __rsub__(self: Self, other: Self | int) -> Self:
        return other + (-self)

    def __truediv__(self: Self, other: Self | int) -> 'RationalFunction':
        return RationalFunction(self, other)

    def __rtruediv__(self: Self, other: Self | int) -> 'RationalFunction':
        return RationalFunction(other, self)

    def __eq__(self: Self, other: object) -> bool:
        if not isinstance(other, (self.__class__, int)):
            return NotImplemented
        
        return not self - other

    def roots(self: Self) -> frozenset[complex]:
        match len(self):
            case 0:
                return frozenset({0})
            case 1:
                return frozenset()
            case 2:
                b, a = self.coeffs
                return frozenset({-b/a})
            case 3:
                c, b, a = self.coeffs
                d = b ** 2 - 4 * a * c
                pr = cmath.sqrt(d)
                rs = frozenset({pr, -pr})
                return frozenset((-b + r) / (2 * a) for r in rs)
            case _:
                raise ValueError('solving polynomials of degree greater than 2 is not supported')

@dataclass
class RationalFunction:
    numerator: Polynomial
    denominator: Polynomial

    def __init__(
        self: Self,
        numerator: Polynomial | int=0,
        denominator: Polynomial | int=1
    ) -> None:

        if isinstance(numerator, int): numerator = Polynomial(numerator)
        if isinstance(denominator, int): denominator = Polynomial(denominator)

        if not denominator:
            raise ValueError('denominator is 0')

    def __bool__(self: Self) -> bool:
        return bool(self.numerator)

    def __call__(self: Self, x: complex) -> complex:
        return self.numerator(x) / self.denominator(x)

    def __str__(self: Self) -> str:
        if self.numerator == 0: return '0'
        numerator_string = str(self.numerator)
        if self.denominator == 1: return numerator_string
        denominator_string = str(self.denominator)
        if len(self.numerator) > 1: numerator_string = f'({numerator_string})'
        if len(self.denominator) > 1: denominator_string = f'({denominator_string})'
        return f'{numerator_string}/{denominator_string}'

    def __add__(self: Self, other: Self | int) -> Self:
        if isinstance(other, int):
            return self + self.__class__(other)

        return self.__class__(
            self.numerator * other.denominator + other.numerator * self.denominator,
            self.denominator * other.denominator
        )

    def __radd__(self: Self, other: Self | int) -> Self:
        return self + other

    def __mul__(self: Self, other: Self | int) -> Self:
        if isinstance(other, int):
            return self * self.__class__(other)

        return self.__class__(
            self.numerator * other.numerator,
            self.denominator * other.denominator
        )

        return self.__class__(
            sum(self[j] * other[i - j] for j in range(i + 1))
            for i in range(len(self) + len(other))
        )

    def __rmul__(self: Self, other: Self | int) -> Self:
        return self * other

    def __neg__(self: Self) -> Self:
        return self * -1

    def __sub__(self: Self, other: Self | int) -> Self:
        return self + (-other)

    def __eq__(self: Self, other: object) -> bool:
        if not isinstance(other, (self.__class__, int)):
            return NotImplemented

        return not self - other

    def roots(self: Self) -> frozenset[complex]:
        return self.numerator.roots() - self.denominator.roots()