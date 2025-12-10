from collections.abc import Generator, Iterable
from dataclasses import dataclass
import itertools as it

test_inputs = [
    ('example', '''\
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}''', [
        ('p1', 7),
        ('p2', 33)
])
]

@dataclass
class Machine:
    goal: list[int]
    buttons: list[frozenset[int]]
    joltages: list[int]

def parse(ip: str) -> Generator[Machine]:
    for line in ip.splitlines():
        parts = line.split()

        goal_str = parts[0]
        assert goal_str[0] == '[' and goal_str[-1] == ']'
        goal = list((1 if c == '#' else 0) for c in goal_str[1:-1])

        joltage_str = parts[-1]
        assert joltage_str[0] == '{' and joltage_str[-1] == '}'
        joltages = list(map(int, joltage_str[1:-1].split(',')))

        button_strs = parts[1:-1]
        buttons: list[frozenset[int]] = []

        for button_str in button_strs:
            assert button_str[0] == '(' and button_str[-1] == ')'
            button = frozenset(map(int, button_str[1:-1].split(',')))
            buttons.append(button)

        yield Machine(goal, buttons, joltages)

def press(lights: list[int], button: frozenset[int]) -> list[int]:
    return [(1 - v if i in button else v) for i, v in enumerate(lights)]

def press_all(
    lights: list[int], buttons: Iterable[frozenset[int]]
) -> list[int]:

    for button in buttons:
        lights = press(lights, button)

    return lights

def minimal_presses(machine: Machine) -> int:
    lights = [0] * len(machine.goal)
    buttons = machine.buttons

    for button_count in range(len(buttons) + 1):
        combos = it.combinations(buttons, button_count)

        for combo in combos:
            combo_set = frozenset(combo)

            if press_all(lights, combo_set) == machine.goal:
                return button_count
            
    assert False

def p1(ip: str) -> int:
    # so the state of the machine can be thought of as a bit string
    # each button, when pressed, is toggling the bits at certain positions on
    # or off --- i.e. is XORing the bit string with a certain mask
    # our task is to find a combination of masks such that XORing the string
    # with all these masks produces the desired output
    # we can probably take advantage of associativity here

    # we don't need to worry about pressing a button twice or more --- that
    # just undoes the effect

    # so we can consider all possible combinations of buttons without
    # replacement, which is a finite set

    # we know initially all the bits are 0
    # look at the goal and see what bits we need to turn on
    # 

    #    [....]
    #    [.#.#]
    #    [.##.]
    #    [#.#.]
    #    [.##.]

    machines = list(parse(ip))
    return sum(map(minimal_presses, machines))

def press_p2(joltages: list[int], button: frozenset[int]) -> list[int]:
    return [(v + 1 if i in button else v) for i, v in enumerate(joltages)]

def press_all_p2(
    joltages: list[int], buttons: Iterable[frozenset[int]]
) -> list[int]:

    for button in buttons:
        joltages = press_p2(joltages, button)

    return joltages

def minimal_presses_p2(machine: Machine) -> int:
    joltages = [0] * len(machine.joltages)
    buttons = machine.buttons

    # ok, this time it's not enough to just check combinations, because
    # multiplicities are also relevant
    # is this a system of lienar equations in disguise?

    for button_count in range(len(buttons) + 1):
        combos = it.combinations(buttons, button_count)

        for combo in combos:
            combo_set = frozenset(combo)

            if press_all_p2(joltages, combo_set) == machine.joltages:
                return button_count
            
    assert False

def p2(ip: str) -> int:
    return sum(minimal_presses_p2(machine) for machine in parse(ip))