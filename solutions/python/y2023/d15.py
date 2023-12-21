from collections import defaultdict
import re
from typing import Optional

test_inputs = [
	('hash_example', 'HASH', [
		('hash_alg', '52')
	]),
	(
		'example',
		'rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7',
		[
			('hashes_csv', '30,253,97,47,14,180,9,197,48,214,231'),
			('p1', '1320'),
			('p2', '145'),
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

def parse_step(step: str) -> tuple[str, str, Optional[int]]:
	m = re.match(r'^(\w+)(-|=\d+)$', step)
	if m is None: breakpoint()
	label, op = m.groups()

	if op[0] == '=':
		arg = int(op[1:])
		op = '='
	else:
		arg = None

	return label, op, arg
	box = hash_alg(label)

def focusing_power(box_no: int, slot_no: int, foc_len: int) -> int:
	return (box_no + 1) * (slot_no + 1) * foc_len

def p2(ip: str) -> int:
	steps = parse(ip)
	# a lens has a label and a focus length, and is located at a certain slot, in a certain box
	# boxes is a hash keying each box by its box number
	# each box is a hash keying each lens in the box by its label
	# the entry for a lens is a tuple (slot_no, foc_len)
	boxes = defaultdict(lambda: {})

	for step in steps:
		print(boxes)
		label, op, arg = parse_step(step)
		box_no = hash_alg(label)

		if op == '-':
			if label in boxes[box_no]:
				slot_no, _ = boxes[box_no][label]
				del boxes[box_no][label]

				for other_label, (other_slot_no, other_foc_len) in boxes[box_no].items():
					if other_slot_no > slot_no:
						boxes[box_no][other_label] = other_slot_no - 1, other_foc_len
		elif op == '=':
			foc_len = arg
			assert isinstance(foc_len, int)

			if label in boxes[box_no]:
				slot_no, _ = boxes[box_no][label]
				boxes[box_no][label] = slot_no, foc_len
			else:
				slot_no = max(slot_no for slot_no, _ in boxes[box_no].values()) + 1 if boxes[box_no] else 0
				boxes[box_no][label] = slot_no, foc_len

	lenses = []

	for box_no, box_lenses in boxes.items():
		for label, (slot_no, foc_len) in box_lenses.items():
			lenses.append((box_no, slot_no, foc_len))

	return sum(focusing_power(*lens) for lens in lenses)