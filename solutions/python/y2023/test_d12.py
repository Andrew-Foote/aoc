from solutions.python.y2023.d12 import (
	group_locs_valid, poss_group_locs, poss_group_loc_sets,
	repl_from_group_locs, Row, unfold_row, unfold_sizes, valid_group_loc_sets
)

def test_poss_group_locs() -> None:
	assert poss_group_locs(Row.parse('???.###'), 3, 4) == (4,)
	assert poss_group_locs(Row.parse('???.###'), 3, 0) == (0, 4)

def test_poss_group_loc_sets() -> None:
	assert poss_group_loc_sets(Row.parse('???.###'), (1, 1, 3)) == [(0, 2, 4)]
	
	assert poss_group_loc_sets(Row.parse('.??..??...?##.'), (1, 1, 3)) == [
		(1, 5, 10),
		(1, 6, 10),
		(2, 5, 10),
		(2, 6, 10),
	]

	row = Row.parse('?###????????')
	sizes = (3, 2, 1)

	assert [
		str(repl_from_group_locs(row, locs, sizes))
		for locs in poss_group_loc_sets(row, sizes)
	] == [
		'.###.##.#...',
		'.###.##..#..',
		'.###.##...#.',
		'.###.##....#',
		'.###..##.#..',
		'.###..##..#.',
		'.###..##...#',
		'.###...##.#.',
		'.###...##..#',
		'.###....##.#',
	]

	assert poss_group_loc_sets(Row.parse('.??#??#?#?#?.??????'), (9, 1, 1)) == [
		(2, 13, 15), (2, 13, 16), (2, 13, 17), (2, 13, 18), (2, 14, 16),
		(2, 14, 17), (2, 14, 18), (2, 15, 17), (2, 15, 18), (2, 16, 18),
		(3, 13, 15), (3, 13, 16), (3, 13, 17), (3, 13, 18), (3, 14, 16),
		(3, 14, 17), (3, 14, 18), (3, 15, 17), (3, 15, 18), (3, 16, 18)
	]

def test_unfold_row() -> None:
	assert unfold_row(Row.parse('.#')) == Row.parse('.#?.#?.#?.#?.#')

	assert unfold_row(Row.parse('???.###')) == Row.parse(
		'???.###????.###????.###????.###????.###'
	)

def test_unfold_sizes() -> None:
	assert unfold_sizes((1,)) == (1, 1, 1, 1, 1)
	assert unfold_sizes((1, 1, 3)) == (1,1,3,1,1,3,1,1,3,1,1,3,1,1,3)

def test_group_locs_valid() -> None:
	assert not group_locs_valid(Row.parse('#.'), (1,), (1,))

def test_valid_group_locs() -> None:
	assert sum(
		1 for _ in valid_group_loc_sets(
			unfold_row(Row.parse('?###????????')), unfold_sizes((3, 2, 1))
		)
	) == 506250