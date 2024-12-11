from collections.abc import Generator

test_inputs = [
    ('example', '0 1 10 99 999', [
        ('state_1_pic', '1 2024 1 0 9 9 2021976'),
    ]),
    ('example2', '125 17', [
        ('state_1_pic', '253000 1 7'),
        ('stone_count_6', 22),
        ('p1', 55312)
    ])
]

def parse(ip: str) -> list[int]:
    return list(map(int, ip.split()))

def transform(stones: list[int]) -> list[int]:
    new_stones: list[int] = []

    for stone in stones:
        if stone == 0:
            new_stones.append(1)
            continue

        digits = str(stone)
        digit_count = len(digits)
        q, r = divmod(digit_count, 2)

        if r:
            new_stones.append(stone * 2024)
        else:
            new_stones.extend([int(digits[:q]), int(digits[q:])])
            continue

    return new_stones

def states(stones: list[int]) -> Generator[list[int]]:
    yield stones

    while True:
        stones = transform(stones)
        yield stones

def state_n(stones: list[int], n: int) -> list[int]:
    for i, stones in enumerate(states(stones)):
        if i == n:
            return stones

def state_1_pic(ip: str) -> str:
    return ' '.join(map(str, state_n(parse(ip), 1)))

def stone_count_n(stones: list[int], n: int) -> int:
    return len(state_n(stones, n))

def stone_count_6(ip: str) -> int:
    return stone_count_n(parse(ip), 6)

def p1(ip: str) -> int:
    return stone_count_n(parse(ip), 25)

    # with each blink
    # - any stones engraved with 0 turn into stones engraved with 1
    # - any stones engraved with numbers with even number of digits
    #   split into two, with the digits split in half.
    #     new numbers will have even numbers of digits except for
    #     leading zeros
    # - any stones engraved with positive odd numbers have their number
    #   multiplied by 2024

    # floor(log_10(n)) + 1
    # floor(log_10(n * 2024)) + 1
    # = floor(log_10 n + log_10 2024) + 1
    #   since log_10 2024 =~ 3, floor(log_10 n + log_10 2024)
    #     can be either floor(log_10 n) + 3 or floor(log_10 n) + 4
    #     so floor(log_10(n * 2024)) + 1 is either floor(log_10 n) + 3
    # so multiplying by 2024 adds either 3 or 4 digits

    # we want to count the number of stones


    # x - number of stones with zeros,
    # y - number of stones with even number of digits
    # z - number of stones with positive odd numbers of digits
    #
    # x(t + 1) = [number of stones in y(t) where the right half of the digits are all 0, let's call that y0(t)]
    # y0(t + 1) = [number of stones in y(t) with 4 or more digits, where the right quarter of the digits are all 0 -- y1(t)]
    # y1(t + 1) = 