test_inputs = [('code_grid_height', '6', [
     ('code_grid', '''\
   | 1   2   3   4   5   6   
---+---+---+---+---+---+---+
 1 |  1   3   6  10  15  21
 2 |  2   5   9  14  20
 3 |  4   8  13  19
 4 |  7  12  18
 5 | 11  17
 6 | 16''')
 ])]

import re

def parse(ip: str) -> tuple[int, int]:
    m = re.match(
        r'To continue, please consult the code grid in the manual.  Enter the '
        r'code at row (\d+), column (\d+).',
        ip
    )

    assert m is not None, f'"{ip}"'
    row, col = m.groups()
    return int(row), int(col)

def row_and_col_to_index(row: int, col: int) -> int:
    # 2 = 1 + 1
    #     1
    # 3 = 1 + 2; 2 + 1
    #     3      2
    # 4 = 3 + 1; 2 + 2; 1 + 4
    #     6      5      4
    # 5 = 4 + 1; 3 + 2; 2 + 3; 4 + 1
    #     10     9      8      7
    #
    # 1, 3, 6, 10, ... are triangular numbers

    n = row + col
    s = (n * (n - 1)) // 2
    return s - row + 1

def code_grid(ip: str) -> str:
    height = int(ip)
    
    lines: list[str] = [
        '   | ' + ''.join(f'{j:<4}' for j in range(1, height + 1)),
        '---+' * (height + 1),
    ]

    for i in range(1, height + 1):
        width = height - i + 1
        cols = [row_and_col_to_index(i, j) for j in range(1, width + 1)]
        lines.append(f' {i} | ' + '  '.join(f'{col:>2}' for col in cols))

    return '\n'.join(lines)

def p1(ip: str) -> int:
    row, col = parse(ip)
    n = row_and_col_to_index(row, col)
    return 20151125 * pow(252533, n - 1, 33554393)