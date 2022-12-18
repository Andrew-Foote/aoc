from fractions import Fraction
import math
import numbers
import operator
import sys
from typing import Self, SupportsComplex, Type

def _operator_fallbacks(monomorphic_operator, fallback_operator):
    # stolen from the fractions module

    def forward(a, b):
        if isinstance(b, (int, gint)):
            return monomorphic_operator(a, b)
        elif isinstance(b, Fraction) and b.denominator == 1:
            return monomorphic_operator(a, b.numerator)
        elif isinstance(b, (float, complex)):
            return fallback_operator(complex(a), complex(b))
        else:
            return NotImplemented
    forward.__name__ = f'__{fallback_operator.__name__}__'
    forward.__doc__ = monomorphic_operator.__doc__

    def reverse(b, a):
        if isinstance(a, gint) or isinstance(a, numbers.Integral):
            return monomorphic_operator(a, b)
        elif isinstance(a, numbers.Rational) and a.denominator == 1:
            return monomorphic_operator(a.numerator, b)
        elif isinstance(a, numbers.Complex):
            return fallback_operator(complex(a), complex(b))
        else:
            return NotImplemented
    reverse.__name__ = f'__r{fallback_operator.__name__}__'
    reverse.__doc__ = monomorphic_operator.__doc__

    return forward, reverse

class gint(numbers.Complex):
    """A Gaussian integer, i.e. a complex number whose real and imaginary parts are integers.
    
    In terms of implementation, the real and imaginary parts are stored as Python `int` objects,
    rather than `float` objects like the built-in `complex` class. This means they can be
    arbitrarily large and operations on them are exact.
    
    Operations are defined to be closed. For example, if you try to add 0.5 to gint(1, 1), you get
    a ValueError because 0.5 + 1j would not be a Gaussian integer. Because of this gint is not
    a subclass of numbers.Complex (for the same reason Decimal isn't)."""

    __slots__ = ['_real', '_imag']
    _real: int
    _imag: int

    def __new__(cls: Type[Self], real: SupportsComplex=0, imag: SupportsComplex=0) -> Self:
        """
        >>> gint(6)
        gint(6, 0)
        >>> gint(2, 3)
        gint(2, 3)
        >>> gint(2.5, 3.7)
        gint(2, 3)
        >>> gint(2 + 3j)
        gint(2, 3)
        >>> gint(2 + 3j, 1 - 6j)
        gint(8, 4)
        >>> gint(gint(2, 3))
        gint(2, 3)
        """
        self = super().__new__(cls)
        a = complex(real)
        b = complex(imag)
        self._real = int(a.real - b.imag)
        self._imag = int(a.imag + b.real)
        return self

    @property
    def real(self: Self) -> int:
        return self._real

    @property
    def imag(self: Self) -> int:
        return self._imag

    def __repr__(self: Self) -> str:
        return f'{self.__class__.__name__}({self.real}, {self.imag})'

    def __str__(self: Self) -> str:
        return str(complex(self))

    def _add(z, w):
        return gint(z.real + w.real, z.imag + w.imag)

    __add__, __radd__ = _operator_fallbacks(_add, operator.add)
    
    def _sub(z, w):
        return gint(z.real - w.real, z.imag - w.imag)

    __sub__, __rsub__ = _operator_fallbacks(_sub, operator.sub)

    def _mul(z, w):
        return gint(z.real * w.real - z.imag * w.imag, z.imag * w.real + z.real * w.imag)

    __mul__, __rmul__ = _operator_fallbacks(_mul, operator.mul)

    def _truediv(z, w):
        return complex(z) / complex(w)

    __truediv__, __rtruediv__ = _operator_fallbacks(_truediv, operator.truediv)

    def _floordiv(z, w):
        """
        >>> gint(7, 2) // 2
        gint(3, 1)
        >>> gint(7, 4) // gint(1, 3)
        gint(1, -2)
        """
        d = norm(w)

        return gint(
            (z.real * w.real + z.imag * w.imag) // d,
            (z.imag * w.real - z.real * w.imag) // d
        )

    __floordiv__, __rfloordiv__ = _operator_fallbacks(_floordiv, operator.floordiv)

    def _divmod(z, w):
        d = norm(w)

        q1, r1 = divmod(z.real * w.real + z.imag * w.imag, d)
        q2, r2 = divmod(z.imag * w.real - z.real * w.imag, d)
        return gint(q1, q2), gint(r1, r2)

    __divmod__, __rdivmod__ = _operator_fallbacks(_divmod, divmod)

    def _mod(z, w):
        # this might not be right - doesn't satisfy z = qw + r
        d = norm(w)

        return gint(
            (z.real * w.real + z.imag * w.imag) % d,
            (z.imag * w.real - z.real * w.imag) % d
        )

    __mod__, __rmod__ = _operator_fallbacks(_mod, operator.mod)

    def _pow(z, w):
        # (a + bi)^n = sum_(k = 0)^n i^k a^(n - k) b^k
        if isinstance(w, int):
            parts = [0, 0]

            for k in range(w + 1):
                t = math.comb(w, k) * z.real ** (w - k) * z.imag ** k
                r = k % 4
                parts[r % 2] += (-1) ** (r // 2) * t

            a, b = parts
            return gint(a, b)

        return complex(z) ** complex(w)

    __pow__, __rpow__ = _operator_fallbacks(_pow, operator.pow)

    def __pos__(self: Self) -> Self:
        return self.__class__(self.real, self.imag)

    def __neg__(self: Self) -> Self:
        return self.__class__(-self.real, -self.imag)

    def conjugate(self: Self) -> Self:
        return self.__class__(self.real, -self.imag)

    def norm(self: Self) -> int:
        return self.real ** 2 + self.imag ** 2

    def __abs__(self: Self) -> float: # type: ignore
        return math.sqrt(self.norm())

    def __complex__(self: Self) -> complex:
        return complex(self.real, self.imag)

    def __hash__(self: Self) -> int:
        # See https://docs.python.org/3/library/stdtypes.html#numeric-hash
        r = hash(self.real) + sys.hash_info.imag * hash(self.imag)
        r %= 2 ** sys.hash_info.width
        return -2 if r == -1 else r

    def __eq__(self: Self, other: object) -> bool:
        if isinstance(other, numbers.Complex):
            return self.real == other.real and self.imag == other.imag
        
        return NotImplemented

    def __bool__(self: Self) -> bool:
        return bool(self.real) or bool(self.imag)

    def __copy__(self: Self) -> Self:
        if type(self) == gint:
            return self
        
        return self.__class__(self.real, self.imag)

    def __deepcopy__(self: Self) -> Self:
        if type(self) == gint:
            return self

        return self.__class__(self.real, self.imag)

def norm(z: gint | int) -> int:
    if isinstance(z, gint):
        return z.norm()
    else:
        return z ** 2

def gint_rect(z: gint) -> tuple[int, int]:
    return z.real, z.imag 

if __name__ == '__main__':
    import doctest
    doctest.testmod()