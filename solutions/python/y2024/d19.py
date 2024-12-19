from collections.abc import Generator
from enum import Enum
import functools as ft

test_inputs = [('example', '''\
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb''', [
    ('decompositions_csv', '''\
br,wr,r
b,g,g,r
gb,br
r,rb,g,br

bwu,r,r,g
br,g,r
'''),
    ('p1', 6),
])]

class Colour(Enum):
    WHITE = 'w'
    BLUE = 'u'
    BLACK = 'b'
    RED = 'r'
    GREEN = 'g'

def parse_colour_list(s: str) -> tuple[Colour, ...]:
    return tuple(map(Colour, s))

def format_colour_list(colours: tuple[Colour, ...]) -> str:
    return ''.join(colour.value for colour in colours)

def parse(ip: str) -> tuple[
    set[tuple[Colour, ...]], list[tuple[Colour, ...]]
]:
    patterns_s, designs_s = ip.split('\n\n')
    patterns = {parse_colour_list(s.strip()) for s in patterns_s.split(',')}
    designs = [parse_colour_list(s.strip()) for s in designs_s.splitlines()]
    return patterns, designs

def decompositions(
    design: tuple[Colour, ...], patterns: set[tuple[Colour, ...]]
) -> Generator[list[tuple[Colour, ...]]]:
    
    if not design:
        yield []
    
    if design in patterns:
        yield [design]
    
    for i in range(1, len(design)):
        prefix = design[:i]
        suffix = design[i:]
        prefix_decomps = decompositions(prefix, patterns)
        suffix_decomps = decompositions(suffix, patterns)
        
        for prefix_decomp in prefix_decomps:
            for suffix_decomp in suffix_decomps:
                yield prefix_decomp + suffix_decomp

def is_possible(
    design: tuple[Colour, ...], patterns: set[tuple[Colour, ...]]
) -> bool:
    
    # input()
    # print(f'DESIGN {format_colour_list(design)}')

    @ft.cache
    def recurse(design: tuple[Colour, ...]) -> bool:
        # print(f'   recurse({format_colour_list(design)})')
        if not design:
            return True
        
        if design in patterns:
            return True
        
        for i in range(1, len(design)):
            if recurse(design[:i]) and recurse(design[i:]):
                return True
        
        return False
    
    return recurse(design)

def decompositions_csv(ip: int) -> str:
    patterns, designs = parse(ip)
    lines: list[str] = []

    for design in designs:
        decomps = decompositions(design, patterns)
        decomp_strings: list[str] = []

        for decomp in decomps:
            decomp_strings.append(','.join(
                format_colour_list(c) for c in decomp
            ))

        lines.append(' '.join(decomp_strings))

    return '\n'.join(lines)

def p1(ip: str) -> int:
    # we need to know how many designs are possible
    # a design is possible if it can be produced by concatenation of
    # available patterns (where a pattern can occur any number of times,
    # provided it's available)

    patterns, designs = parse(ip)
    result = 0

    for design in designs:
        if is_possible(design, patterns):
            result += 1

    return result