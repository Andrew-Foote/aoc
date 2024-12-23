from collections.abc import Generator
import itertools as it

test_inputs = [('singular-example', '''\
123''', [
    ('next_10_secret_numbers', '''\
15887950
16495136
527345
704524
1553684
12683156
11100544
12249484
7753432
5908254'''),
]), ('example', '''\
1
10
100
2024''', [
    ('report', '''\
1: 8685429
10: 4700978
100: 15273692
2024: 8667524'''),
    ('p1', 37327623)
]), ('p2-example', '''\
1
2
3
2024''', [
    ('p2', '-2,1,-1,3'),
])]

def mix(num: int, val: int) -> int:
    return num ^ val

def prune(num: int) -> int:
    return num % 16777216

def next_secret_number(cur: int) -> int:
    cur = mix(cur, cur * 64)
    cur = prune(cur)
    cur = mix(cur, cur // 32)
    cur = prune(cur)
    cur = mix(cur, cur * 2048)
    cur = prune(cur)
    return cur

def next_secret_numbers(cur: int) -> Generator[int]:
    while True:
        cur = next_secret_number(cur)
        yield cur

def parse(ip: str) -> list[int]:
    # initial secret number of each buyer
    return list(map(int, ip.splitlines()))

def next_10_secret_numbers(ip: str) -> str:
    num, = parse(ip)
    return '\n'.join(str(n) for n in it.islice(next_secret_numbers(num), 10))

def get_secret_number(initial_num: int, index: int) -> int:
    return next(
        n for i, n in enumerate(next_secret_numbers(initial_num))
        if i >= index - 1
    )

def report(ip: str) -> str:
    lines: list[str] = []

    for num in parse(ip):
        nxt = get_secret_number(num, 2000)
        lines.append(f'{num}: {nxt}')

    return '\n'.join(lines)

def p1(ip: str) -> int:
    return sum(get_secret_number(num, 2000) for num in parse(ip))

def price(num: int) -> int:
    return num % 10

def price_changes(cur_num: int) -> Generator[int]:
    prev_price = price(cur_num)

    while True:
        cur_num = next_secret_number(cur_num)
        cur_price = price(cur_num)
        yield cur_price - prev_price
        prev_price = cur_price

def p2(ip: str) -> int:
    nums = parse(ip)

    for num in nums:
        for n in it.islice(price_changes(num), 2000):
            ...
            


# we want to sell it at the highest price
# to do that we need to instruct the monkey to sell it
# after the right sequence of 4 consecutive changes in price