from dataclasses import dataclass
import re
from typing import Iterator, Optional

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
])]

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

@dataclass
class AlmanacMap:
	src: str
	dst: str
	lines: list[MapLine]

	def apply(self, val: int) -> int:
		for line in self.lines:
			print('APPLYING', line, 'TO', val)
			new = line.apply(val)

			if new is not None:
				print('LINE WAS APPLIED')
				return new

		print('NO LINE APPLIED, RETURNING UNCHANGED')
		return val

def parse(ip: str) -> tuple[list[int], list[AlmanacMap]]:
	m = re.match(r'^seeds:(.*)', ip)
	if m is None: breakpoint()
	seeds, = m.groups()
	seeds = map(int, seeds.split())
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
	print('LOCATION FOR SEED', seed)
	cur = 'seed'
	val = seed
	maps_by_src = {mapp.src: mapp for mapp in maps}

	while cur in maps_by_src:
		mapp = maps_by_src[cur]
		print('CUR=', cur, 'VAL=', val, 'NXT=', mapp.dst)
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