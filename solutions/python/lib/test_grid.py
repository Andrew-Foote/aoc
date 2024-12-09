from hypothesis import given
from hypothesis import strategies as st
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Rect

def gints(*,
    min_real: int | None=None, max_real: int | None=None,
    min_imag: int | None=None, max_imag: int | None=None
) -> st.SearchStrategy:
    
    return st.builds(
        gint, st.integers(min_real, max_real), st.integers(min_imag, max_imag)
    )

@st.composite
def rects(draw: st.DrawFn) -> Rect:
    top = draw(st.integers())
    left = draw(st.integers())
    bottom = draw(st.integers(top))
    right = draw(st.integers(left))
    return Rect(top, right, bottom, left)

def old_rect_add_impl(rect: Rect, disp: gint) -> Rect:
    return rect.__class__(
        rect.top + disp.imag, rect.right + disp.real,
        rect.bottom + disp.imag, rect.left + disp.real
    )

@given(st.lists(gints(min_real=-3, max_real=3, min_imag=-3, max_imag=3), min_size=1))
def test_rect_bounding(points: list[gint]) -> None:
    rect = Rect.bounding(points)

    rect_points = set(rect)
    assert set(points).issubset(rect_points)

    bounding_points = set()
    top = min(point.imag for point in points)
    bottom = max(point.imag for point in points)
    left = min(point.real for point in points)
    right = max(point.real for point in points)

    for i in range(top, bottom + 1):
        for j in range(left, right + 1):
            bounding_points.add(gint(j, i))

    assert rect_points == bounding_points

@given(rects(), gints())
def test_rect_add(rect: Rect, disp: gint) -> None:
    assert rect + disp == old_rect_add_impl(rect, disp)