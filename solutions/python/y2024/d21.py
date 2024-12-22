test_inputs = [('example', '''\
''', [
    ('p1', 0),
])]

def parse(ip: str) -> list[str]:
    return ip.splitlines()

def numeric_part_of_code(code: str) -> int:
    return int(code[:-1])

def complexity(code: str) -> int:
    return numeric_part_of_code(code) * min_presses_for_code(code)

def p1(ip: str) -> int:
    codes = parse(ip)
    return sum(complexity(code) for code in codes)
    # we need to sum the complexities of each code
    # complexity = l * n
    # where l is the length of the shortest sequence of
    # button presses on the directional keypad that will
    # cause the code to be typed on the numeric keypad
    # and n is the numeric part of the code
    # it seems that every code has the form "DDDA" where
    # D is a digt

    # so, we control a directional keypad
    # 