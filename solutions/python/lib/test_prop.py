from solutions.python.lib.prop import alphalits, dnf

def test_dnf() -> None:
	a, b, c, e, f, g, h, i, j, *_ = alphalits()
	
	assert dnf(
		((a & b) | c)
		& ((e & f & g) | h | (i & j))
	) == [
		[a, b, e, f, g], [a, b, h], [a, b, i, j],
		[c, e, f, g], [c, h], [c, i, j]
	]