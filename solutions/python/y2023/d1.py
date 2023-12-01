import operator

test_inputs = [('example', '''\
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet\
''', [
	('calibration_values_p1_csv', '12,38,15,77')
]),
('example2', '''\
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen\
''', [
	('calibration_values_p2_csv', '29,83,13,24,42,14,76')
])]

def calibration_values_p1(ip: str) -> list[int]:
	vals = []

	for line in ip.splitlines():
		digits = [int(c) for c in line if c.isdigit()]
		vals.append(digits[0] * 10 + digits[-1])

	return vals

def calibration_values_p1_csv(ip: str) -> str:
	return ','.join(map(str, calibration_values_p1(ip)))

def p1(ip: str) -> int:
	return sum(calibration_values_p1(ip))

DIGIT_SYMBOLS = ('1', '2', '3', '4', '5', '6', '7', '8', '9')
DIGIT_WORDS = ('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine')

DIGITS = {s: i + 1 for i, s in enumerate(DIGIT_SYMBOLS)} | {w: i + 1 for i, w in enumerate(DIGIT_WORDS)}

def first_digit(line, backwards=False):
	bestpos = None
	digit = None

	index_method = 'rindex' if backwards else 'index'

	for digit_string, digit_value in DIGITS.items():
		try:
			pos = getattr(line, index_method)(digit_string)
		except ValueError:
			continue
		else:
			cmp = operator.gt if backwards else operator.lt

			if bestpos is None or cmp(pos, bestpos):
				bestpos = pos
				digit = digit_value

	return digit

def calibration_values_p2(ip: str) -> list[int]:
	vals = []

	for line in ip.splitlines():
		d0 = first_digit(line, False)
		d1 = first_digit(line, True)
		vals.append(d0 * 10 + d1)

	return vals

def calibration_values_p2_csv(ip: str) -> str:
	return ','.join(map(str, calibration_values_p2(ip)))

def p2(ip: str) -> int:
	return sum(calibration_values_p2(ip))
