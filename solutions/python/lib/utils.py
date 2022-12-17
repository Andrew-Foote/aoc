def range_includes(bigrange: range, lilrange: range) -> bool:
	return bigrange.start <= lilrange.start and bigrange.stop >= lilrange.stop

def range_intersection(r1: range, r2: range) -> range:
	return range(max(r1.start, r2.start), min(r1.stop, r2.stop))