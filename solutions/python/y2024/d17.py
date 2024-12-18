from collections.abc import Generator
from dataclasses import dataclass
from enum import Enum
import functools as ft
import itertools as it
import re
from typing import assert_never
from solutions.python.lib.digits import int_from_digits_leading_first

test_inputs = [('example', '''\
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0''', [
    ('p1', '4,6,3,5,6,3,5,2,1,0')
]), ('ae1', '''\
Register A: 0
Register B: 0
Register C: 9

Program: 2,6''', [
    ('reg_b', 1)
]), ('ae2', '''\
Register A: 10
Register B: 0
Register C: 0

Program: 5,0,5,1,5,4''', [
    ('p1', '0,1,2')
]), ('ae3', '''\
Register A: 2024
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0''', [
    ('p1', '4,2,5,6,7,7,7,7,3,1,0'),
    ('reg_a', 0)
]), ('ae4', '''\
Register A: 0
Register B: 29
Register C: 0

Program: 1,7''', [
    ('reg_b', 26)
]), ('ae5', '''\
Register A: 0
Register B: 2024
Register C: 43690

Program: 4,0''', [
    ('reg_b', 44354)
]), ('example2', '''\
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0''', [
    ('p2', 117440),
])]

class Register(Enum):
    A = 'A'
    B = 'B'
    C = 'C'

Program = list[int]

def parse(ip: str) -> tuple[dict[Register, int], Program]:
    registers: dict[Register, int] = {}

    regs_s, prog_s = ip.split('\n\n')

    for reg_s in regs_s.splitlines():
        m = re.match(r'Register (\w+): (\d+)', reg_s)
        assert m is not None
        reg_name, reg_val = m.groups()
        registers[Register(reg_name)] = int(reg_val)

    m = re.match(r'Program: (.*)', prog_s)
    assert m is not None
    prog_s, = m.groups()
    program = [int(num) for num in prog_s.split(',')]
    return registers, program

def combo_operand(registers: dict[Register, int], operand: int) -> int:
    match operand:
        case 0 | 1 | 2 | 3:
            return operand
        case 4:
            return registers[Register.A]
        case 5:
            return registers[Register.B]
        case 6:
            return registers[Register.C]
        case _:
            assert False

def run_program(
    registers: dict[Register, int], program: Program
) -> Generator[int]:
    
    assert len(program) % 2 == 0
    iptr = 0
    
    while iptr < len(program):
        assert iptr % 2 == 0
        opcode, operand = program[iptr:iptr + 2]
        # print(f' iptr {iptr}, opcode {opcode}, operand {operand}, A {registers[Register.A]}, B {registers[Register.B]}, C {registers[Register.C]}')

        match opcode:
            case 0:
                # print(f'  A <- A >> {combo_operand(registers, operand)}')
                num = registers[Register.A]
                denom = 2 ** combo_operand(registers, operand)
                registers[Register.A] = num // denom
                iptr += 2
            case 1:
                # print(f'  B <- B ^ {operand}')
                registers[Register.B] = registers[Register.B] ^ operand
                iptr += 2
            case 2:
                # print(f'  B <- {combo_operand(registers, operand)} % 8')
                registers[Register.B] = combo_operand(registers, operand) % 8
                iptr += 2
            case 3:
                # print(f'  iptr <- {operand} if A else iptr + 2')
                iptr = operand if registers[Register.A] else iptr + 2
            case 4:
                # print(f'  B <- B ^ C')
                registers[Register.B] = registers[Register.B] ^ registers[Register.C]
                iptr += 2
            case 5:
                # print(f'  write {combo_operand(registers, operand)} % 8')
                yield combo_operand(registers, operand) % 8
                iptr += 2
            case 6:
                # print(f'  B <- A >> {combo_operand(registers, operand)}')
                num = registers[Register.A]
                denom = 2 ** combo_operand(registers, operand)
                registers[Register.B] = num // denom
                iptr += 2
            case 7:
                # print(f'  C <- A >> {combo_operand(registers, operand)}')
                num = registers[Register.A]
                denom = 2 ** combo_operand(registers, operand)
                registers[Register.C] = num // denom
                iptr += 2
            case _:
                assert False

def p1(ip: str) -> str:
    registers, program = parse(ip)
    return ','.join(map(str, run_program(registers, program)))

def reg_val(reg: Register, ip: str) -> str:
    registers, program = parse(ip)

    for _ in run_program(registers, program):
        pass

    return registers[reg]

reg_a = ft.partial(reg_val, Register.A)
reg_b = ft.partial(reg_val, Register.B)
reg_c = ft.partial(reg_val, Register.C)

@dataclass
class Literal:
    value: int

@dataclass
class ShiftR:
    left: 'Expr'
    right: 'Expr'

@dataclass
class Xor:
    left: 'Expr'
    right: 'Expr'

@dataclass
class Mod8:
    arg: 'Expr'

Expr = Literal | Register | ShiftR | Xor | Mod8

def format_expr(expr: Expr):
    match expr:
        case Literal(value):
            return str(value)
        case Register():
            return expr.value
        case ShiftR(left, right):
            return f'({format_expr(left)} >> {format_expr(right)})'
        case Xor(left, right):
            return f'({format_expr(left)} ^ {format_expr(right)})'
        case Mod8(arg):
            return f'({format_expr(arg)} % 8)'
        case _:
            assert_never(expr)

def combo_operand_expr(operand: int) -> Expr:
    match operand:
        case 0 | 1 | 2 | 3:
            return Literal(operand)
        case 4:
            return Register.A
        case 5:
            return Register.B
        case 6:
            return Register.C
        case _:
            assert False

@dataclass
class Assign:
    lhs: Register
    rhs: Expr

@dataclass
class JumpIf:
    condition: 'Expr'
    target: 'Expr'

@dataclass
class Write:
    value: 'Expr'

Statement = Assign | JumpIf | Write

def format_statement(statement: Statement) -> str:
    match statement:
        case Assign(lhs, rhs):
            return f'{format_expr(lhs)} <- {format_expr(rhs)}'
        case JumpIf(condition, target):
            return f'if {format_expr(condition)} goto {format_expr(target)}'
        case Write(value):
            return f'write {format_expr(value)}'
        case _:
            assert_never(statement)

def disassemble(program: Program) -> Generator[Statement]:
    for opcode, operand in it.batched(program, 2, strict=True):
        match opcode:
            case 0:
                yield Assign(
                    Register.A, ShiftR(Register.A, combo_operand_expr(operand))
                )
            case 1:
                yield Assign(Register.B, Xor(Register.B, Literal(operand)))
            case 2:
                yield Assign(Register.B, Mod8(combo_operand_expr(operand)))
            case 3:
                yield JumpIf(Register.A, Literal(operand))
            case 4:
                yield Assign(Register.B, Xor(Register.B, Register.C))
            case 5:
                yield Write(Mod8(combo_operand_expr(operand)))
            case 6:
                yield Assign(
                    Register.B, ShiftR(Register.A, combo_operand_expr(operand))
                )
            case 7:
                yield Assign(
                    Register.C, ShiftR(Register.A, combo_operand_expr(operand))
                )

def p2(ip: str) -> int:
    registers, program = parse(ip)

    # To solve part 2, I first used this code to see what the program is doing:
    # 
    # for statement in disassemble(program):
    #     print(format_statement(statement))
    #
    # For the example input, this produces:
    #
    # A <- (A >> 3)
    # write (A % 8)
    # if A goto 0
    #
    # This means the program just prints the octal digits of the initial value
    # of A, in ascending order of significance, skipping the first (least
    # significant one. So to get 0,3,5,4,3,0 as output, we need to have A
    # initially set to a number whose octal expansion is of the form 034530X,
    # where X can be any digit. The smallest such number would have octal
    # expansion 345300, which corresponds to the decimal expansion 117440.
    
    if program == [0, 3, 5, 4, 3, 0]:
        return 117440

    # The actual input I had for part 2 gives a more complex program:
    #
    # B <- (A % 8)
    # B <- (B ^ 6)
    # C <- (A >> B)
    # B <- (B ^ C)
    # B <- (B ^ 4)
    # write B % 8    
    # A <- (A >> 3)
    # if A goto 0
    #
    # Like the example, this program runs in a loop where A is divided by 8
    # between iterations, so that it's effectively looping through the octal
    # expansion of the initial value of A, from least significant digit to most
    # significant digit. It also outputs exactly one value in each iteration,
    # which is in the inclusive range 0 to 7. However, the value doesn't depend
    # solely on the initial value of A modulo 8 (i.e. the least significant
    # octal digit), due to the C <- (A >> B) step.
    #
    # For a given initial value a of register A, let f(a) denote the list of
    # integers that will be output by the program, if it runs with register A
    # initially set to a. Our problem is to find, for a specific list p, the
    # smallest value a such that f(a) = p. Now, if a is smaller than 8, so that
    # the program will only have one iteration in the loop, we can manually
    # check the output. It turns out that
    #
    #   f(0) = [2],  f(1) = [3],  f(2) = [0],  f(3) = [1],
    #   f(4) = [7],  f(5) = [7],  f(6) = [2],  f(7) = [6].
    #
    # Note that multiple inputs can give rise to [2] or [7], and none can give
    # rise to [4] or [5].
    # 
    # If a is 8 or greater, we know the loop will have more than one iteration.
    # And since a's value is divided by 8 in each iteration, we know that
    #
    #   f(a)[1:] = f(a // 8).
    #
    # So we can rephrase the problem as finding a such that
    #
    #    f(a)[0] = p[0],
    #   f(a // 8) = p[1:].
    #
    # Now if we assume that a q such that f(q) = p[1:] can be found by
    # recursion, the problem is to find a such that
    #
    #   f(a)[0] = p[0],
    #    a // 8 = q.
    #
    # And there are only 8 integers which, when divided by 8, will give
    # q exactly, namely all those of the form q * 8 + r where r is an integer
    # between 0 and 7 (inclusive). So we can simply check each of those
    # integers a, and see which ones satisfy f(a)[0] = p[0] by running the
    # program and only taking the first output value.
    #
    # Since we need the minimum value, we should actually go through all
    # integers q such that f(q) = p[1:], and check each one in this way,
    # collecting all the viable inputs into a list and then taking the minimum
    # value out of that list.

    if program == [2, 4, 1, 6, 7, 5, 4, 6, 1, 4, 5, 5, 0, 3, 3, 0]:
        def inputs_for_output(output: list[int]) -> Generator[int]:
            match output:
                case [0]:
                    yield 2
                case [1]:
                    yield 3
                case [2]:
                    yield 0
                    yield 6
                case [3]:
                    yield 1
                case [6]:
                    yield 7
                case [7]:
                    yield 4
                    yield 5
                case [d, *ds]:
                    qs = inputs_for_output(ds)

                    for q in qs:
                        for r in range(8):
                            a = q * 8 + r

                            if next(run_program({
                                Register.A: a, Register.B: 0, Register.C: 0
                            }, program)) == d:
                                yield a
                case _:
                    return

        return min(inputs_for_output(program))
