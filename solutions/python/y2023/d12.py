from dataclasses import dataclass
from enum import Enum
import itertools as it
from typing import Iterator, Literal, Self

test_inputs = [
	(
		'no_unknown_example', '''\
#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1''', [
		],
	),
	(
		'example', '''\
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1''', [
			('arrangement_counts_csv', '1,4,1,1,4,10'),
			('p1', 21),
		]
	)
]

class SpringState(Enum):
	OPERATIONAL = '.'
	DAMAGED = '#'
	UNKNOWN = '?'

	def __str__(self) -> str:
		return self.value

	@classmethod
	def parse(cls, c: str) -> Self:
		return cls(c)

KnownSpringState = (
	Literal[SpringState.OPERATIONAL] | Literal[SpringState.DAMAGED]
)

KNOWN_SPRING_STATES = (SpringState.OPERATIONAL, SpringState.DAMAGED)

@dataclass
class Row[T: SpringState]:
	items: list[T]

	def __iter__(self) -> Iterator[T]:
		yield from self.items

	def __str__(self) -> str:
		return ''.join(map(str, self.items))		

UnknownRow = Row[SpringState]
KnownRow = Row[KnownSpringState]

def parse(ip: str) -> Iterator[tuple[UnknownRow, list[int]]]:
	lines = ip.splitlines()

	for line in lines:
		row_s, sizes_s = line.split()
		row = Row(list(map(SpringState.parse, row_s)))
		sizes = list(map(int, sizes_s.split(',')))
		yield row, sizes

def poss_repls(row: UnknownRow) -> Iterator[KnownRow]:
	"""Yields all rows obtainable by replacing all the unknown spring states
	within the original row with known spring states."""

	unknowns = [
		i for i, state in enumerate(row) if state == SpringState.UNKNOWN
	]

	for irepls in it.product(KNOWN_SPRING_STATES, repeat=len(unknowns)):
		repl = row.items.copy()

		for i, irepl in zip(unknowns, irepls):
			repl[i] = irepl

		yield Row(repl)

def get_sizes(row: KnownRow) -> list[int]:
	result: list[int] = []
	counter: int = 0

	for state in [*row, SpringState.OPERATIONAL]:
		match state:
			case SpringState.OPERATIONAL:
				if counter:
					result.append(counter)
					counter = 0
			case SpringState.DAMAGED:
				counter += 1
			case _:
				assert_never(state)

	return result

def poss_repl_is_compatible(row: KnownRow, sizes: list[int]) -> bool:
	return get_sizes(row) == sizes

def compatible_repls(row: UnknownRow, sizes: list[int]) -> Iterator[KnownRow]:
	for repl in poss_repls(row):
		# print(f'  poss repl: {repl}; {get_sizes(repl)}', end=' ')

		if poss_repl_is_compatible(repl, sizes):
			yield repl
			# print('compatible')
		else:
			pass# print('not compatible')


def arrangement_counts(ip: str) -> Iterator[int]:
	for row, sizes in parse(ip):
		# print(f'\nrow: {row}; sizes: {sizes}')
		yield sum(1 for repl in compatible_repls(row, sizes))

def arrangement_counts_csv(ip: str) -> str:
	return ','.join(map(str, arrangement_counts(ip)))

def p1(ip: str) -> int:
	# this is already pretty slow
	return sum(arrangement_counts(ip))


	# #
	# # alternatively, the contiguous groups tell us how many
	# # broken springs we have, and give some information (but
	# # not complete info) about where they are. so for each
	# # one, we can check all the possible locations

	# for size in sizes:
	# 	# what are the possible locations of the group?
	# 	# in absence of considering placement of other
	# 	# groups, they are all those spans within the row
	# 	# that don't contain any operational springs (only
	# 	# broken or unknown)
	# 	for damaged_spring in range(size):
