from solutions.python.y2023.d12 import (
	Row, unfold_row, unfold_sizes
)

def test_unfold_row() -> None:
	assert unfold_row(Row.parse('.#')) == Row.parse('.#?.#?.#?.#?.#')

	assert unfold_row(Row.parse('???.###')) == Row.parse(
		'???.###????.###????.###????.###????.###'
	)

def test_unfold_sizes() -> None:
	assert unfold_sizes((1,)) == (1, 1, 1, 1, 1)
	assert unfold_sizes((1, 1, 3)) == (1,1,3,1,1,3,1,1,3,1,1,3,1,1,3)
