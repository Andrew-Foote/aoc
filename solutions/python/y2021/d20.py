from typing import Iterable

def int_from_bits(bits: Iterable[bool]) -> int:
	# most significant bit first
	v = 0

	for b in bits:
		v |= b
		v <<= 1

	return v

def parse(ip: str) -> list[Alg, Img]:
	alg, img = ip.split('\n\n')
	alg = int.from_bytes()
	return alg, img