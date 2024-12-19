from enum import Enum
import functools as ft

import line_profiler

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
#     ('decompositions_csv', '''\
# b,r,wr,r br,wr,r
# b,g,g,r
# g,b,b,r g,b,br gb,b,r gb,br
# r,r,b,g,b,r r,r,b,g,br r,r,b,gb,r r,rb,g,b,r r,rb,g,br r,rb,gb,r

# bwu,r,r,g
# b,r,g,r br,g,r
# '''),
#     ('p1', 6),
#     ('p2', 16),
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

def decomp_count(
    design: tuple[Colour, ...], patterns: set[tuple[Colour, ...]]
) -> int:

    print(f'DESIGN {format_colour_list(design)}')

    @ft.cache
    def recurse(design: tuple[Colour, ...]) -> int:
        if design:
            result = 0

            if design in patterns:
                print(f'    {format_colour_list(design)} in pattersn')
                result += 1

            for i in range(1, len(design)):
                # the + double-counts decomps in cases like
                # rgr -> 
                #        rg, r -> r, g, r
                #        r, gr -> r, g, r
                # where r, g, r is counted twice
                result += recurse(design[:i]) * recurse(design[i:])
        else:
            result = 1

        print(f'   recurse({format_colour_list(design)}) = {result}')
        return result
    
    return recurse(design)

def decompositions(
    design: tuple[Colour, ...], patterns: set[tuple[Colour, ...]]
) -> set[tuple[tuple[Colour, ...], ...]]:

    # print(f'DESIGN {format_colour_list(design)}')
    # input()

    @ft.lru_cache(maxsize=32768)
    @line_profiler.profile
    def recurse(
        design: tuple[Colour, ...]
    ) -> set[tuple[tuple[Colour, ...], ...]]:

        # print(f'   recurse({format_colour_list(design)})')

        if not design:
            return set()
        
        result: set[tuple[tuple[Colour, ...], ...]] = set()

        if design in patterns:
            result.add((design,))
        
        for i in range(1, len(design)):
            prefix = design[:i]
            suffix = design[i:]
            prefix_decomps = recurse(prefix)
            suffix_decomps = recurse(suffix)

            if (not prefix_decomps) or (not suffix_decomps):
                continue
            
            for prefix_decomp in prefix_decomps:
                for suffix_decomp in suffix_decomps:
                    result.add(prefix_decomp + suffix_decomp)

        return result

    return recurse(design)

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
        decomps = sorted(decompositions(design, patterns))
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

def p2(ip: str) -> int:

    patterns, designs = parse(ip)
    result = 0

    for design in designs:
        # result += decomp_count(design, patterns)
        result += len(decompositions(design, patterns))

    return result