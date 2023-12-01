from collections import Counter
from dataclasses import dataclass
import re
from utils import joinlines
from solutions.python.lib.grid import Rect

test_inputs = [
	('example_p1', joinlines([
		'#1 @ 1,3: 4x4',
		'#2 @ 3,1: 4x4',
		'#3 @ 5,5: 2x2'
	]), [
		('p1', '4'),
		('p2', '3')
	])
]

@dataclass(frozen=True)
class Claim:
	id_: int
	rect: Rect

def parse_claim(claim: str) -> Claim:
	m = re.match(r'#(\d+) @ (\d+),(\d+): (\d+)x(\d+)', claim)
	if m is None: raise RuntimeError
	claim_id, left, top, width, height = map(int, m.groups())
	return Claim(claim_id, Rect.from_tlwh(top, left, width, height))

def p1(ip: str) -> int:
	claims = map(parse_claim, ip.splitlines())
	claimed = Counter()

	for claim in claims:
		for p in claim.rect:
			claimed[p] += 1

	return sum(1 for p, k in claimed.items() if k > 1)

def p2(ip: str):
	claims = set(map(parse_claim, ip.splitlines()))
	candidates = claims.copy()
	claimed = {}

	for claim in claims:
		for p in claim.rect:
			if p in claimed:
				if claim in candidates:
					candidates.remove(claim)
				if claimed[p] in candidates:
					candidates.remove(claimed[p])
			else:
				claimed[p] = claim

	print(candidates)
	assert len(candidates) == 1
	return candidates.pop().id_
