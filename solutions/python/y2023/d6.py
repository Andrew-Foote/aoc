from dataclasses import dataclass
import math
from typing import Iterator
from solutions.python.lib.utils import prod

test_inputs = [('example', '''\
Time:      7  15   30
Distance:  9  40  200
''', [
	('races_csv', '7,9;15,40;30,200'),
	('ways_to_beat_csv','2,5;4,11;11,19'),
	('ways_to_beat_count_csv','4,8,9'),
	('p1','288'),
	('p2','71503')
])]

@dataclass
class Race:
	duration: int
	record_distance: int

def parse(ip: str) -> Iterator[Race]:
	times_line, distances_line = ip.splitlines()
	times = map(int, times_line.split()[1:])
	distances = map(int, distances_line.split()[1:])

	for time, distance in zip(times, distances):
		yield Race(time, distance)

def races_csv(ip: str) -> str:
	return ';'.join(f'{race.duration},{race.record_distance}' for race in parse(ip))

def distance(race_duration: int, hold_time: int) -> int:
	remaining_time = race_duration - hold_time
	return remaining_time * hold_time

def ways_to_beat(race: Race) -> range:
	# t = race duration
	# h = hold time
	# d = distance travelled
	# d = h * (t - h) = ht - h^2
	# c = record distance
	# we want to find cardinaity of { h in ZZ : ht - h^2 > c }
	# i.e. { h in ZZ : ht - h^2 >= c + 1 }
	# i.e. { h in ZZ : (-1) h^2 + th - (c + 1) >= 0 }
	# this will be the interval between the two roots of that quadratic
	# equation
	# which are
	# [-t +- sqrt(t^2 - 4 * (-1) * (-(c + 1)))]/(2 * (-1))
	# i.e. [t -+ sqrt(t^2 - 4(c + 1))]/2
	# i.e. [t -+ sqrt(t^2 - 4c - 4)]/2

	def root(sign):
		t = race.duration
		c = race.record_distance
		return (t + sign * math.sqrt(t**2 - 4*(c + 1))) / 2

	r1 = root(-1)
	r2 = root(1)
	rmin, rmax = (r1, r2) if r1 <= r2 else (r2, r1)
	return range(math.ceil(rmin), math.floor(rmax) + 1)

def ways_to_beat_csv(ip: str) -> str:
	ranges = [ways_to_beat(race) for race in parse(ip)]
	return ';'.join(f'{r.start},{r.stop - 1}' for r in ranges)

def ways_to_beat_count(race: Race) -> int:
	return len(ways_to_beat(race))

def ways_to_beat_count_csv(ip: str) -> str:
	return ','.join(str(ways_to_beat_count(race)) for race in parse(ip))

def p1(ip: str) -> int:
	return prod(ways_to_beat_count(race) for race in parse(ip))

def parse_p2(ip: str) -> Race:
	times_line, distances_line = ip.splitlines()
	time = int(''.join(times_line.split()[1:]))
	distance = int(''.join(distances_line.split()[1:]))
	return Race(time, distance)

def p2(ip: str) -> int:
	return ways_to_beat_count(parse_p2(ip))