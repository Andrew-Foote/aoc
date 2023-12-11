from dataclasses import dataclass
import re
from typing import Iterator, Optional
from solutions.python.lib.rangeset import RangeSet

# is it just dijkstra?
# we need to get the minimum location value
# but the location value is not a path length... is it?

test_inputs = [('example', '''\
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
''', [
	('locations_csv', '82,43,86,35'),
	('p1','35'),
	('p2','46')
])]

humidity to location:
(56, 93) -> (60, 97)
(93, 97) -> (56, 60)

so 

# how do we get to the lowest location?
# well the lowest location would be 0
# but it might not be reachable
# maybe we can start iterating from 0, and just find the first one that's reachable

@dataclass
class MapLine:
	drs: int # destination range start
	srs: int # source range start
	rl: int # range length

	def apply(self, val: int) -> Optional[int]:
		if self.srs <= val < self.srs + self.rl:
			diff = val - self.srs
			return self.drs + diff

		return None

	# apply to a range?
	def apply_to_range(self, r: range) -> None:


	@property
	def srng(self) -> range:
		return range(self.srs, self.srs + self.rl)

	@property
	def drng(self) -> range:
		return range(self.drs, self.drs + self.rl)

@dataclass
class AlmanacMap:
	src: str
	dst: str
	lines: list[MapLine]

	def apply(self, val: int) -> int:
		for line in self.lines:
			#print('APPLYING', line, 'TO', val)
			new = line.apply(val)

			if new is not None:
				#print('LINE WAS APPLIED')
				return new

		#print('NO LINE APPLIED, RETURNING UNCHANGED')
		return val

	def not_fixed_rangeset(self, val: int) -> RangeSet:
		return RangeSet.from_ranges(line.srng() for line in self.lines)

	def img_of_not_fixed_rangeset(self, val: int) -> RangeSet:
		return RangeSet.from_ranges(line.drng() for line in self.lines)

	def preimage(self, rng: range) -> RangeSet:
		ranges = [rng]

		for line in self.lines:
			# let A = rng.start, B.rng.stop, f = the function corresponding to the line
			# this function adds D = (line.drs - line.srs) to values in the interval (C, D)
			# [C = line.srs, D = line.srs + line.rl], and fixes other values
			# the question is, when is A <= f(x) < B
			# well, for C <= x <= D we have f(x) = x + D
			# hence A <= f(x) < B equiv A <= x + D < B equiv A - D <= x < B - D
			# otherwise, A <= f(x) < B equiv A < x < B
			ranges.append(range(rng.start - (line.drs - line.srs), range(rng.stop - (line.drs - line.srs))))

		return RangeSet.from_ranges(ranges)

	def compose(self, other: 'AlmanacMap') -> 'AlmanacMap':
		"""
		f = map which maps (3, 4) to (5, 6) and (5, 7) to (2, 4)
		composed with
		g = map which maps (4, 8) to (1, 5)
		=
		well, g can also be expressed as:
			map which maps (4, 6) to (1, 3), (6, 7) to (3, 4), (7, 8) to (4, 5)
			hence, composing with g gives:
				map which maps (4, 6) to (1, 3), (6, 7) to (5, 6), (7, 8) to (4, 5)
					plus (5, 6) to (2, 5) --- minus (6, 7) because that was already accounted for
					plus (3, 4) to (5, 6)

		so we take the outer function
			look at all the ranges it affects
			find the preimage of each affected range under the inner function
				preimage of (3, 4) under g = (3, 4) uu (6, 7)
				preimage of (5, 7) under g = O/
			elements in those preimages will be hit twice:
				(3, 4) ->g-> (3, 4) ->f-> (4, 5)

			any element not in one of the preimages, we only need to apply inner function

		>>> m1 = AlmanacMap(MapLine(5, 3, 1), MapLine(2, 5, 2)
		>>> m2 = AlmanacMap(MapLine(1, 4, 4))
		>>> compose(m1, m2)
		AlmanacMap(MapLine())
		"""

		lines = []

		for line in self.lines:
			r = line.srng()
			p = other.preimage(r)

			for r2 in p.ranges():
				lines.append(MapLine(line.apply(r2.start), r2.start, r2.stop - r2.start))
	 
		return AlmanacMap(lines)

def parse(ip: str) -> tuple[list[int], list[AlmanacMap]]:
	m = re.match(r'^seeds:(.*)', ip)
	if m is None: breakpoint()
	seeds, = m.groups()
	seeds = list(map(int, seeds.split()))
	rest = ip[m.end():].strip()
	rest = rest.split('\n\n')
	maps = []

	for mapstr in rest:
		mapstr = mapstr.strip()
		m = re.match(r'(\w+)-to-(\w+) map:', mapstr)
		if m is None: breakpoint()
		src, dst = m.groups()
		rest = mapstr[m.end():].strip()
		rest = rest.splitlines()
		lines = []

		for linestr in rest:
			linestr = linestr.strip()
			drs, srs, rl = map(int, linestr.split())
			lines.append(MapLine(drs, srs, rl))

		maps.append(AlmanacMap(src, dst, lines))

	return seeds, maps

def location_for_seed(maps: list[AlmanacMap], seed: int) -> int:
	#print('LOCATION FOR SEED', seed)
	cur = 'seed'
	val = seed
	maps_by_src = {mapp.src: mapp for mapp in maps}

	while cur in maps_by_src:
		mapp = maps_by_src[cur]
		#print('CUR=', cur, 'VAL=', val, 'NXT=', mapp.dst)
		val = mapp.apply(val)
		cur = mapp.dst

	assert cur == 'location'
	return val

def locations(ip: str) -> Iterator[int]:
	seeds, maps = parse(ip)
	
	for seed in seeds:
		yield location_for_seed(maps, seed)

def locations_csv(ip: str) -> str:
	return ','.join(map(str, locations(ip)))

def p1(ip: str) -> int:
	return min(locations(ip))

def reinterpret_for_p2(seeds: list[int]) -> list[range]:
	for start, length in zip(seeds[::2], seeds[1::2]):
		yield range(start, start + length)

def p2(ip: str) -> int:
	seeds, maps = parse(ip)
	seeds = reinterpret_for_p2(seeds)
	locations = []

	for seed_range in seeds:
		for seed in seed_range:
			locations.append(location_for_seed(maps, seed))

	# ok we only need the minimum
	# 
	return min(locations)