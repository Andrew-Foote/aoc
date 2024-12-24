from enum import Enum
from typing import assert_never
import graphviz
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
    ('z_wire_output', '0001011111100'),
    ('p1', 2024)
])]

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

def p2(ip: str) -> int:
    inputs, eqns = parse(ip)

    eqns['shh'], eqns['z21'] = eqns['z21'], eqns['shh']
    eqns['dtk'], eqns['vgs'] = eqns['vgs'], eqns['dtk']
    eqns['dqr'], eqns['z33'] = eqns['z33'], eqns['dqr']
    eqns['pfw'], eqns['z39'] = eqns['z39'], eqns['pfw']

    try:
        validate_adder(inputs, eqns)
    except AssertionError as e:
        import traceback
        print(traceback.format_exc())

    g = graphviz.Digraph()

    for rhs, (operator, lopnd, ropnd) in eqns.items():
        g.node(rhs, label=f'{rhs}: {operator.value}')
        g.node(lopnd)
        g.node(ropnd)
        g.edge(lopnd, rhs)
        g.edge(ropnd, rhs)

    g.render('solutions/python/y2024/d24.gv', view=True)

    return ','.join(sorted((
        'shh', 'z21', 'dtk', 'vgs', 'dqr', 'z33', 'pfw', 'z39'
    )))

def validate_adder(inputs: set[str], eqns: dict[str, Expr]) -> bool:
    groups = [{} for _ in range(45)]
    validated = set()
    node_to_type_and_group: dict[str, tuple[str, int]] = {}

    def validate(rhs: str) -> None:
        if rhs in validated:
            return None

        assert rhs[0] not in 'xy'
        op, left, right = eqns[rhs]

        def is_carry(node_type, num):
            if num:
                return node_type == 'combcarry'
            else:
                return node_type == 'carry'

        match op:
            case Op.XOR: # sum / adjsum
                if (
                    (left[0] == 'x' and right[0] == 'y')
                    or (left[0] == 'y' and right[0] == 'x')
                ): # sum
                    num1 = int(left[1:])
                    num2 = int(right[1:])
                    assert num1 == num2

                    if num1 == 0:
                        assert rhs == 'z00'
                    else:
                        assert rhs[0] != 'z'

                    assert 'sum' not in groups[num1], (rhs, op, left, right, groups[num1])
                    groups[num1]['sum'] = rhs
                    node_to_type_and_group[rhs] = 'sum', num1
                else: # adjsum
                    validate(left)
                    validate(right)
                    ltype, lgroup = node_to_type_and_group[left]
                    rtype, rgroup = node_to_type_and_group[right]
                    # the first error was here: ('ssh', 21, 20)
                    # third error was here too: ('dqr', 32, 33)
                    # and fourth: ('pfw', 39, 38)
                    assert rhs[0] == 'z', (rhs, lgroup, rgroup)

                    if (
                        rtype == 'sum' and is_carry(ltype, lgroup)
                        and lgroup == rgroup - 1
                    ):
                        assert 'adjsum' not in groups[rgroup]
                        groups[rgroup]['adjsum'] = rhs
                        node_to_type_and_group[rhs] = 'adjsum', rgroup
                    elif (
                        ltype == 'sum' and is_carry(rtype, rgroup)
                        and rgroup == lgroup - 1
                    ):
                        assert 'adjsum' not in groups[lgroup]
                        groups[lgroup]['adjsum'] = rhs
                        node_to_type_and_group[rhs] = 'adjsum', lgroup
                    else:
                        assert False
            case Op.AND: # carry / adjcarry
                if (
                    (left[0] == 'x' and right[0] == 'y')
                    or (left[0] == 'y' and right[0] == 'x')
                ): # carry
                    num1 = int(left[1:])
                    num2 = int(right[1:])
                    assert num1 == num2
                    assert rhs[0] != 'z'
                    assert 'carry' not in groups[num1]
                    groups[num1]['carry'] = rhs
                    node_to_type_and_group[rhs] = 'carry', num1
                else: # adjcarry
                    validate(left)
                    validate(right)
                    ltype, lgroup = node_to_type_and_group[left]
                    rtype, rgroup = node_to_type_and_group[right]
                    assert rhs[0] != 'z'
                     
                    if (
                        rtype == 'sum' and is_carry(ltype, lgroup)
                        and lgroup == rgroup - 1
                    ):
                        assert 'adjcarry' not in groups[rgroup]
                        groups[rgroup]['adjcarry'] = rhs
                        node_to_type_and_group[rhs] = 'adjcarry', rgroup
                    elif (
                        ltype == 'sum' and is_carry(rtype, rgroup)
                        and rgroup == lgroup - 1
                    ):
                        assert 'adjcarry' not in groups[lgroup]
                        groups[lgroup]['adjcarry'] = rhs
                        node_to_type_and_group[rhs] = 'adjcarry', lgroup
                    else:
                        # second error was here: ('bmg', <Op.AND: 'AND'>, 'vgs', 'skh', 'carry', 26, 'combcarry', 25)
                        assert False, (rhs, op, left, right, ltype, lgroup, rtype, rgroup)
            case Op.OR: # combcarry
                validate(left)
                validate(right)
                ltype, lgroup = node_to_type_and_group[left]
                rtype, rgroup = node_to_type_and_group[right]
                assert lgroup == rgroup
                assert lgroup == 44 or rhs[0] != 'z'

                assert (
                    (ltype == 'carry' and rtype == 'adjcarry')
                    or (ltype == 'adjcarry' or rtype == 'carry')
                )

                assert 'combcarry' not in groups[lgroup]
                groups[lgroup]['combcarry'] = rhs
                node_to_type_and_group[rhs] = 'combcarry', lgroup

        validated.add(rhs)

    for rhs in eqns:
        validate(rhs)

    # we can classify nodes in the graph into the following types:
    #
    # x nodes
    #    these have labels of the form 'x{n:02}' where 0 <= n <= 44,
    #    and no in-edges
    #
    # y nodes
    #    these have labels of the form 'y{n:02}' where 0 <= n <= 44,
    #    and no in-edges
    #
    # modular sum nodes
    #    these have two parents, an x and y node with the same number,
    #    and have operator XOR
    #    NB: the modular sum node for x00 and y00 is z00, however others
    #    are not z nodes
    #
    # raw carry nodes
    #    these nodes have two parents, which should be an x and y node
    #    with the same number, and have operator AND
    #
    # carry-adjusted modular sum nodes (z01--z44)
    #    these nodes have two parents, which should be a modular sum
    #    node for some n, and the carry node for n - 1
    #      (for n = 1, the carry node will be a raw carry node, otherwise
    #      it will be a combined carry node)
    #    and operator XOR
    #
    # carry-adjusted carry nodes
    #    these nodes have two parents, which should be a modular sum
    #    node for some n, and the carry node for n - 1
    #      (for n = 1, the carry node will be a raw carry node, otherwise
    #      it will be a combined carry node)
    #    and operator AND
    #
    # combined carry nodes
    #    these are OR nodes with two parents: the raw carry node for n,
    #    and the carry-adjusted carry node for n
    #      z45 is the combined carry node for n = 44
    #
    # for each n from 0 to 44 we expect:
    #   1 x node, 1 y node, 1 modular sum node, 1 raw carry node
    # for nodes 1 to 44 we additionally expect:
    #   1 carry-adjusted modular sum node, 1 carry-adjusted carry node,
    #   1 combined carry node
    # z node should be the modular sum node for 0, the carry-adjsted modular
    #   sum node for 1--44, the combined carry node (of 44) for 45

    #
    # z01--z44
    #       


