test_inputs = [
	('hash_example', 'HASH', [
		('hash_alg', '52')
	]),
	(
		'example',
		'rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7',
		[
			('hashes_csv', '30,253,97,47,14,180,9,197,48,214,231'),
			('p1', '1320')
		]
	)
]

def hash_alg(s: str) -> int:
	cur = 0

	for c in s:
		cur += ord(c)
		cur *= 17
		cur %= 256

	return cur

def parse(ip: str) -> list[str]:
	return ip.replace('\n', '').split(',')

def hashes(ip: str) -> list[int]:
	return map(hash_alg, parse(ip))

def hashes_csv(ip: str) -> str:
	return ','.join(map(str, hashes(ip)))

def p1(ip: str) -> int:
	return sum(hashes(ip))