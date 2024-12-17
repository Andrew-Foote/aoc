from dataclasses import dataclass
import re

test_inputs = [('example', '''\
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0''', [
    ('p1', '4,6,3,5,6,3,5,2,1,0')
])]

def truncdiv(num : int, denom : int):
    if (num < 0) != (denom < 0):
        return -(num // -denom)
    else:
        return num // denom

def div(a, b):
    return truncdiv(a, b)
    # return int(a / b)

def p1(ip: str) -> int:
    # what is the program trying to output?
    # initialize the registers as specialized
    # then run the program
    # join values produced by out instrs with commas
    parts = ip.split('\n\n')
    registers = {}

    for reg_s in parts[0].splitlines():
        m = re.match(r'Register (\w+): (\d+)', reg_s)
        assert m is not None
        reg_name, reg_val = m.groups()
        registers[reg_name] = int(reg_val)

    m = re.match(r'Program: (.*)', parts[1])
    assert m is not None
    prog_s, = m.groups()
    prog = [int(num) for num in prog_s.split(',')]

    # program is a list of numbers from 0 to 7
    iptr = 0

    outputs = []

    while True:
        if iptr >= len(prog):
            break

        assert iptr < len(prog) - 1
        opcode = prog[iptr]
        operand = prog[iptr + 1]

        def combo_operand_val():
            match operand:
                case 0 | 1 | 2 | 3:
                    return operand
                case 4:
                    return registers['A']
                case 5:
                    return registers['B']
                case 6:
                    return registers['C']
                case _:
                    assert False

        match opcode:
            case 0:
                numtor = registers['A']
                denomtor = 2 ** combo_operand_val()
                registers['A'] = div(numtor, denomtor)
                iptr += 2
            case 1:
                registers['B'] = registers['B'] ^ operand
                iptr += 2
            case 2:
                registers['B'] = combo_operand_val() % 8
                iptr += 2
            case 3:
                if registers['A']:
                    iptr = operand
                else:
                    iptr += 2
            case 4:
                registers['B'] = registers['B'] ^ registers['C']
                iptr += 2
            case 5:
                outputs.append(combo_operand_val() % 8)
                iptr += 2
            case 6:
                numtor = registers['A']
                denomtor = 2 ** combo_operand_val()
                registers['B'] = div(numtor, denomtor)
                iptr += 2
            case 7:
                numtor = registers['A']
                denomtor = 2 ** combo_operand_val()
                registers['C'] = div(numtor, denomtor)
                iptr += 2

    return ','.join(map(str, outputs))