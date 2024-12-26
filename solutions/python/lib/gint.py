from collections.abc import Callable
from fractions import Fraction
import math
import numbers
import operator
import sys
from typing import overload, Self, SupportsComplex, SupportsInt, Type

# Can't figure out how to type this properly
type MonomorphicOperator[T] = 'Callable[[gint, gint], T]'
type FallbackOperator[T] = 'Callable[[complex, complex], T]'

def _operator_fallbacks[T](
    monomorphic_operator: MonomorphicOperator[T],
    fallback_operator: FallbackOperator[T]
) -> '''tuple[
    Callable[[gint, gint | int | Fraction | float | complex], T],
    Callable[[gint, SupportsComplex], T]
]''':
    # stolen from the fractions module

    def forward(
        a: 'gint', b: 'gint | int | Fraction | float | complex'
    ) -> T:
    
        if isinstance(b, (gint, int)):
            return monomorphic_operator(a, gint(b))
        elif isinstance(b, Fraction) and b.denominator == 1:
            return monomorphic_operator(a, gint(b.numerator))
        elif isinstance(b, (float, complex)):
            return fallback_operator(complex(a), complex(b))
        else:
            return NotImplemented

    forward.__name__ = f'__{fallback_operator.__name__}__'
    forward.__doc__ = monomorphic_operator.__doc__

    def reverse(b: 'gint', a: SupportsComplex) -> T:
        if isinstance(a, (gint, int)):
            return monomorphic_operator(gint(a), b)
        elif isinstance(a, Fraction) and a.denominator == 1:
            return monomorphic_operator(gint(a.numerator), b)
        elif isinstance(a, SupportsComplex):
            return fallback_operator(complex(a), complex(b))
        else:
            return NotImplemented

    reverse.__name__ = f'__r{fallback_operator.__name__}__'
    reverse.__doc__ = monomorphic_operator.__doc__

    return forward, reverse

def _add(z: 'gint', w: 'gint') -> 'gint':
    return gint(z.real + w.real, z.imag + w.imag)

def _sub(z: 'gint', w: 'gint') -> 'gint':
    return gint(z.real - w.real, z.imag - w.imag)

def _mul(z: 'gint', w: 'gint') -> 'gint':
    return gint(
        z.real * w.real - z.imag * w.imag,
        z.imag * w.real + z.real * w.imag
    )

def _truediv(z: 'gint', w: 'gint') -> complex:
    return complex(z) / complex(w)

def _floordiv(z: 'gint', w: 'gint') -> 'gint':
    d = norm(w)

    return gint(
        (z.real * w.real + z.imag * w.imag) // d,
        (z.imag * w.real - z.real * w.imag) // d
    )

def _divmod(z: 'gint', w: 'gint') -> 'tuple[gint, gint]':
    d = norm(w)

    q1, r1 = divmod(z.real * w.real + z.imag * w.imag, d)
    q2, r2 = divmod(z.imag * w.real - z.real * w.imag, d)
    return gint(q1, q2), gint(r1, r2)

def _mod(z: 'gint', w: 'gint') -> 'gint':
    # this might not be right - doesn't satisfy z = qw + r
    d = norm(w)

    return gint(
        (z.real * w.real + z.imag * w.imag) % d,
        (z.imag * w.real - z.real * w.imag) % d
    )

def _pow(z: 'gint', w: 'gint') -> 'gint | complex':
    # (a + bi)^n = sum_(k = 0)^n i^k a^(n - k) b^k
    if w.imag == 0:
        n = w.real
        parts = [0, 0]

        for k in range(n + 1):
            t = math.comb(n, k) * z.real ** (n - k) * z.imag ** k
            r = k % 4
            parts[r % 2] += (-1) ** (r // 2) * t

        a, b = parts
        return gint(a, b)

    return complex(z) ** complex(w)

class gint:
    """A Gaussian integer, i.e. a complex number whose real and imaginary parts
    are integers.
    
    In terms of implementation, the real and imaginary parts are stored as
    Python `int` objects, rather than `float` objects like the built-in
    `complex` class. This means they can be arbitrarily large and operations on
    them are exact.
    
    Operations are defined to be closed. For example, if you try to add 0.5 to
    gint(1, 1), you get a ValueError because 0.5 + 1j would not be a Gaussian
    integer. Because of this gint is not a subclass of numbers.Complex (for the
    same reason Decimal isn't)."""

    # but it is a subclass of numbers.Complex?    

    __slots__ = ['_real', '_imag']
    _real: int
    _imag: int

    def __new__(cls, real: SupportsComplex=0, imag: SupportsComplex=0) -> Self:
        self = super().__new__(cls)

        # The basic idea is to convert `real` and `imag` to complex numbers
        # a + bi and c + di respectively, then set `self._real` to a - d and
        # `self._imag` to b + c, since
        # (a + bi) + (c + di)i = (a - d) + (b + c)i. However, we try to avoid
        # converting to floating point as much as possible, in order to avoid
        # loss of precision.

        a: int | float
        b: int | float
        c: int | float
        d: int | float

        def _conv(part: SupportsComplex) -> tuple[int | float, int | float]:
            if isinstance(part, int):
                return part, 0
            elif isinstance(part, Fraction) and part.denominator == 1:
                return part.numerator, 0
            elif isinstance(part, gint):
                return part.real, part.imag
            else:
                part = complex(part)
                return part.real, part.imag
            
        a, b = _conv(real)
        c, d = _conv(imag)

        self._real = int(a - d)
        self._imag = int(b + c)

        return self

    @property
    def real(self) -> int:
        return self._real

    @property
    def imag(self) -> int:
        return self._imag

    def rect(self) -> tuple[int, int]:
        """Returns the rectangular form of the Gaussian integer, i.e. the tuple
        (a, b) where a is the real part and b is the imaginary part."""
        return self.real, self.imag

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}({self.real}, {self.imag})'

    def __str__(self) -> str:
        return str(complex(self))

    # __add__, __radd__ = _operator_fallbacks(_add, operator.add)

    @overload
    def __add__(self, other: Self) -> Self:
        ...

    @overload
    def __add__(self, other: int) -> Self:
        ...
    
    def __add__(self, other):
        if isinstance(other, int):
            other = gint(other)

        return _add(self, other)
        
    # __add__, __radd__ = _operator_fallbacks(_add, operator.add)
    __sub__, __rsub__ = _operator_fallbacks(_sub, operator.sub)
    __mul__, __rmul__ = _operator_fallbacks(_mul, operator.mul)
    __truediv__, __rtruediv__ = _operator_fallbacks(_truediv, operator.truediv)
    __pow__, __rpow__ = _operator_fallbacks(_pow, operator.pow)
    __floordiv__, __rfloordiv__ = _operator_fallbacks(_floordiv, operator.floordiv)
    __divmod__, __rdivmod__ = _operator_fallbacks(_divmod, divmod)
    __mod__, __rmod__ = _operator_fallbacks(_mod, operator.mod)

    def __pos__(self) -> Self:
        return self.__class__(self.real, self.imag)

    def __neg__(self) -> Self:
        return self.__class__(-self.real, -self.imag)

    def conjugate(self) -> Self:
        return self.__class__(self.real, -self.imag)

    def norm(self) -> int:
        return self.real ** 2 + self.imag ** 2

    def __abs__(self) -> float: # type: ignore
        return math.sqrt(self.norm())

    def __complex__(self) -> complex:
        return complex(self.real, self.imag)
    
    def __lt__(self, other):
        return (self.real, self.imag) < (other.real, other.imag)

    def __hash__(self) -> int:
        # See https://docs.python.org/3/library/stdtypes.html#numeric-hash
        r = hash(self.real) + sys.hash_info.imag * hash(self.imag)
        r %= 2 ** sys.hash_info.width
        return -2 if r == -1 else r

    def __eq__(self, other: object) -> bool:
        if isinstance(other, gint):
            return self.real == other.real and self.imag == other.imag

        return NotImplemented

    def __bool__(self) -> bool:
        return bool(self.real) or bool(self.imag)

    def __copy__(self) -> Self:
        if type(self) == gint:
            return self
        
        return self.__class__(self.real, self.imag)

    def __deepcopy__(self) -> Self:
        if type(self) == gint:
            return self

        return self.__class__(self.real, self.imag)
    
def norm(z: gint | int) -> int:
    if isinstance(z, gint):
        return z.norm()
    else:
        return z ** 2
