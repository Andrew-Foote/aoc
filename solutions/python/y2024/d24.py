from enum import Enum
from typing import assert_never
from solutions.python.lib.digits import int_from_digits_leading_last

test_inputs = [('example', '''\
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02''', [
    ('wire_output_lines', '''\
z00: 0
z01: 0
z02: 1'''),
    ('z_wire_output', '001'),
    ('p1', 4),
]), ('larger-example', '''\
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj''', [
    ('wire_output_lines', '''\
bfw: 1
bqk: 1
djm: 1
ffh: 0
fgs: 1
frj: 1
fst: 1
gnj: 1
hwm: 1
kjc: 0
kpj: 1
kwq: 0
mjb: 1
nrd: 1
ntg: 0
pbm: 1
psh: 1
qhw: 1
rvg: 0
tgd: 0
tnw: 1
vdt: 1
wpb: 0
z00: 0
z01: 0
z02: 0
z03: 1
z04: 0
z05: 1
z06: 1
z07: 1
z08: 1
z09: 1
z10: 1
z11: 0
z12: 0'''),
    ('z_wire_output', '0011111101000'),
    ('p1', 2024)
])]



# x00: 1
# x01: 1
# x02: 1
# y00: 0
# y01: 1
# y02: 0

# x00 AND y00 -> z00
# x01 XOR y01 -> z01
# x02 OR y02 -> z02''', [

class Op(Enum):
    AND = 'AND'
    OR = 'OR'
    XOR = 'XOR'

    def apply(self, x: bool, y: bool) -> bool:
        match self:
            case Op.AND:
                return x and y
            case Op.OR:
                return x or y
            case Op.XOR:
                return x ^ y
            case _:
                assert_never(self)

Expr = tuple[Op, str, str]

def parse(ip: str) -> tuple[dict[str, bool], dict[str, Expr]]:
    inputs_s, eqns_s = ip.split('\n\n')
    inputs: dict[str, int] = {}
    eqns: dict[str, tuple[Op, str, str]] = {}

    for line in inputs_s.splitlines():
        wire, val_s = line.split(':')
        inputs[wire] = bool(int(val_s.strip()))

    for line in eqns_s.splitlines():
        expr, rhs = line.split('->')
        rhs = rhs.strip()
        lopnd, optor, ropnd = expr.split()
        op = Op(optor)
        eqns[rhs] = op, lopnd, ropnd

    print(f'INPUTS: {inputs}')
    input()

    return inputs, eqns

def wait_for_wire_value(
    known_values: dict[str, bool], out_wire: str, eqns: dict[str, Expr]
) -> None:
    
    if out_wire in known_values:
        return

    operator, lop, rop = eqns[out_wire]
    wait_for_wire_value(known_values, lop, eqns)
    wait_for_wire_value(known_values, rop, eqns)
    assert lop in known_values
    assert rop in known_values
    
    result = operator.apply(known_values[lop], known_values[rop])
    print(f'calculating {out_wire} = {lop} {operator.value} {rop} = {result}')
    print(f'  {lop} = {known_values[lop]}')
    print(f'  {rop} = {known_values[rop]}')
    known_values[out_wire] = result

def wire_outputs(ip: str) -> dict[str, bool]:
    result, eqns = parse(ip)

    for rhs, lhs in eqns.items():
        if rhs[0] == 'z':
            wait_for_wire_value(result, rhs, eqns)

    return result

def wire_output_lines(ip: str) -> str:
    inputs, eqns = parse(ip)

    return '\n'.join(
        f'{wire}: {int(output)}'
        for wire, output in sorted(wire_outputs(ip).items())
        if wire not in inputs
    )

def z_wire_bits(ip: str) -> list[bool]:
    z_outputs = (
        (int(wire[1:]), output)
        for wire, output in wire_outputs(ip).items()
        if wire[0] == 'z'
    )

    return [int(output) for _, output in sorted(z_outputs)]

def z_wire_output(ip: str) -> str:
    return ''.join(map(str, z_wire_bits(ip)))

def p1(ip: str) -> int:
    return int_from_digits_leading_last(z_wire_bits(ip), 2)