from hypothesis import given
from hypothesis import strategies as st
from solutions.python.lib.gint import gint

def test_gint() -> None:
    assert gint(6) == gint(6, 0)
    assert gint(2 + 3j) == gint(2, 3)
    assert gint(2 + 3j, 1 - 6j) == gint(8, 4)
    assert gint(gint(2, 3)) == gint(2, 3)

def test_gint_floordiv() -> None:
    assert gint(7, 2) // 2 == gint(3, 1)
    assert gint(7, 4) // gint(1, 3) == gint(1, -2)

@given(st.integers())
def test_gint_on_int(n: int) -> None:
    assert gint(n).rect() == (n, 0)