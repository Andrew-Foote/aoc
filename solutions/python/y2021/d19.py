# I find the description of this puzzle completely incomprehensible and am therefore not doing it.

test_inputs = [('example', '''\
--- scanner 0 ---
0,2
4,1
3,3

--- scanner 1 ---
-1,-1
-5,0
-2,1\
''', [
('complete_map', '''\
...B..
B....S
....B.
S.....\
''')
])]

# each reading is (x, y, z)
# where scannerpos + (x, y, z) = beaconpos
# and this covers all beacons at most 1000 units away on each axis
Reading = tuple[str, str, str]

# we don't know the scanner positions, only their readings
# we don't know the beacon positions either
Scanner = list[Reading]

def parse(ip: str) -> list[Scanner]:
	scanners = []
	
	for section in ip.split('\n\n'):
		readings = []

		for line in section.splitlines()[1:]:
			readings.append(tuple(map(int, line.split(','))))

		scanners.append(section)

	return scanners