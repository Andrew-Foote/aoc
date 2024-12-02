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
        ('p2_safe_reports_csv','0,3,4,5'),
        ('p2', 4),
    ])
]

Report = list[int]

def parse(ip: str) -> Iterator[Report]:
    for line in ip.splitlines():
        yield list(map(int, line.split()))

def report_is_safe(report: Report) -> bool:
    pairs = list(zip(report, report[1:]))

    return (
        (all(l1 < l2 for l1, l2 in pairs) or all(l1 > l2 for l1, l2 in pairs))
        and all(1 <= abs(l1 - l2) <= 3 for l1, l2 in pairs)
    )

def safe_reports(ip: str) -> Iterator[int]:
    for i, report in enumerate(parse(ip)):
        if report_is_safe(report):
            yield i

def safe_reports_csv(ip: str) -> str:
    return ','.join(map(str, safe_reports(ip)))

def p1(ip: str) -> int:
    return len(list(safe_reports(ip)))

def dampenings(report: Report) -> Iterator[Report]:
    for i in range(len(report)):
        yield report[:i] + report[i + 1:]

def p2_safe_reports(ip: str) -> Iterator[int]:
    for i, report in enumerate(parse(ip)):
        for dampening in dampenings(report):
            if report_is_safe(dampening):
                yield i
                break

def p2_safe_reports_csv(ip: str) -> str:
    return ','.join(map(str, p2_safe_reports(ip)))

def p2(ip: str) -> int:
    return len(list(p2_safe_reports(ip)))