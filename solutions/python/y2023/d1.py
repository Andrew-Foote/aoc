test_inputs = [('example', '''\
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
''', [
	('calibration_values_csv', '12,38,15,77')
])]

def calibration_values(ip: str) -> list[int]:
	vals = []

	for line in ip.splitlines():
		digits = [int(c) for c in line if c.isdigit()]
		vals.append(digits[0] * 10 + digits[-1])

	return vals

def calibration_values_csv(ip: str) -> str:
	return ','.join(map(str, calibration_values(ip)))

def p1(ip: str) -> int:
	return sum(calibration_values(ip))