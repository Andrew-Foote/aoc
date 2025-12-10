from collections.abc import Generator, Iterable
from dataclasses import dataclass
import functools as ft
import itertools as it
import operator
from solutions.python.lib import linopt

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
    # ok, this time it's not enough to just check combinations, because
    # multiplicities are also relevant
    # is this a system of lienar equations in disguise?

    # let's call the number of presses for each button x_1, ..., x_n
    # the final joltage value will x_1 Y_0 + ... + x_n Y_n
    # where Y_0, .., Y_n are vectors, with length m equal to the number of
    # joltage counters, and where each component is either 0 or 1, corresponding
    # to buttons
    # we want to solve x_1 Y_0 + ... + x_n Y_n = Y
    # componentwise, this cashes out to m linear equations
    # where each equation corresponds to a joltage counter, each term within an
    # equation corresponds to what a specific button does to this joltage 
    # counter (coeff = 1 if it increments it, 0 otherwise)

    # but, there will probably be multiple solutions
    # oh, and we have to have an integer solution
    #
    # I guess we can do it as a linear programming problem instead:
    # maximise (-x_1) + ... + (-x_n), subject to the constraint that
    # Ax = b

    coeffs: list[list[int]] = []

    for joltage_counter_index in range(len(machine.joltages)):
        coeffs_for_this_counter: list[int] = []

        for button in machine.buttons:
            coeff = 1 if joltage_counter_index in button else 0
            coeffs_for_this_counter.append(coeff)

        coeffs.append(coeffs_for_this_counter)

    # Variables representing the number of times each button was pressed
    press_count_vars = [
        linopt.var(f'b{i}') for i in range(len(machine.buttons))
    ]

    # We want to minimize the total number of button presses
    objective = ft.reduce(operator.add, press_count_vars, linopt.const(0))

    # Subject to the constraint that, for each joltage counter...
    constraints: list[linopt.Constraint] = [
        linopt.ge(x, linopt.const(0)) for x in press_count_vars
    ]

    for joltage_index, goal_joltage in enumerate(machine.joltages):
        # the number of button presses of buttons that increment this particular
        # counter is equal to the goal value
        relevant_press_count_vars = [
            x for button_index, x in enumerate(press_count_vars)
            if joltage_index in machine.buttons[button_index]
        ]

        lhs = ft.reduce(
            operator.add, relevant_press_count_vars, linopt.const(0)
        )

        rhs = linopt.const(goal_joltage)
        constraints.append(linopt.eq(lhs, rhs))

    print()
    print(f'Objective = {objective}')
    print('Constraints:')

    for constraint in constraints:
        print(f'  {constraint}')

    # input()

    result_args, result_val = linopt.solve(objective, constraints)
    return int(result_val)

def p2(ip: str) -> int:
    return sum(minimal_presses_p2(machine) for machine in parse(ip))