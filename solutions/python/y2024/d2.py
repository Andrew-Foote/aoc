from typing import Iterator

test_inputs = [
	('example', '''\
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9''', [
		('safe_reports_csv','0,5'),
		('p1', 2),
	])
]

Report = list[int]

def parse(ip: str) -> Iterator[Report]:
	for line in ip.splitlines():
		yield list(map(int, line.split()))

def report_is_safe(report: Report) -> bool:
	return (
		(
			all(l1 < l2 for l1, l2 in zip(report, report[1:]))
			or
			all(l1 > l2 for l1, l2 in zip(report, report[1:]))
		)
		and all(1 <= abs(l1 - l2) <= 3 for l1, l2 in zip(report, report[1:]))
	)

def safe_reports(ip: str) -> Iterator[int]:
	for i, report in enumerate(parse(ip)):
		if report_is_safe(report):
			yield i

def safe_reports_csv(ip: str) -> str:
	return ','.join(map(str, safe_reports(ip)))

def p1(ip: str) -> int:
	return len(list(safe_reports(ip)))