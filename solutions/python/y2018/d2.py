# adapted from solution written in 2018

from collections import Counter
import string
from typing import Iterator
from utils import joinlines

test_inputs = [
    ('example_p1', joinlines([
        'abcdef', 'bababc', 'abbcde',
        'abcccd', 'aabcdd', 'abcdee',
        'ababab'
    ]), [
        ('counts_csv', joinlines([
            ',', 'a,b', 'b,', ',c', 'ad,', 'e,', ',ab'
        ])),
        ('p1', '12'),
    ]),
    ('example_p2', joinlines([
        'abcde', 'fghij', 'klmno', 'pqrst',
        'fguij', 'axcye', 'wvxyz'
    ]), [
        ('differs_by_one_csv', 'fghij,fguij'),
        ('p2', 'fgij')
    ])
]

def counts(ip: str) -> Iterator[tuple[str, str]]:
    for line in ip.splitlines():
        counter = Counter()

        for c in line.strip():
            counter[c] += 1

        two = ''.join(sorted(c for c, k in counter.items() if k == 2))
        three = ''.join(sorted(c for c, k in counter.items() if k == 3))
        yield two, three

def counts_csv(ip: str) -> str:
    return joinlines([f'{two},{three}' for two, three in counts(ip)])

def twice_count(ip: str) -> int:
    return sum(1 for two, three in counts(ip) if two)

def thrice_count(ip: str) -> int:
    return sum(1 for two, three in counts(ip) if three)

def p1(ip: str) -> int:
    two_count = 0
    three_count = 0

    for two, three in counts(ip):
        two_count += bool(two)
        three_count += bool(three)

    return two_count * three_count

def differs_by_one(ip: str) -> tuple[str, str]:
    identifiers = set()

    for identifier in ip.splitlines():
        for i, c in enumerate(identifier):
            for substitution in string.ascii_lowercase:
                if substitution != c:
                    other_identifier = identifier[:i] + substitution + identifier[i + 1:]

                    if other_identifier in identifiers:
                        return other_identifier, identifier, i

        identifiers.add(identifier)

def differs_by_one_csv(ip: str) -> str:
    id1, id2, i = differs_by_one(ip)
    return f'{id1},{id2}'

def p2(ip: str) -> str:
    id1, id2, i = differs_by_one(ip)
    return id1[:i] + id2[i + 1:]
