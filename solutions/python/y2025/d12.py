from collections.abc import Generator
from dataclasses import dataclass, field
import functools as ft
from solutions.python.lib.grid2 import Point, Rect, Vec

test_inputs = [('example', '''\
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2''', [
    ('first_shape_variant_pics', '''\
###
##.
##.

###
###
..#

.##
.##
###

#..
###
###

###
.##
.##

..#
###
###

##.
##.
###

###
###
#..'''),
    #('p1', 2)
])]

@dataclass(frozen=True, slots=True)
class Shape:
    index: int
    tiles: frozenset[Vec]

    WIDTH = 3
    HEIGHT = 3

    @staticmethod
    def from_lines(index: int, lines: list[str]) -> 'Shape':
        tiles: set[Vec] = set()

        for y, line in enumerate(lines):
            for x, char in enumerate(line):
                if char == '#':
                    tiles.add(Vec(x, y))

        return Shape(index, frozenset(tiles))
    
    def picture(self) -> str:
        lines: list[str] = []

        for y in range(self.HEIGHT):
            chars: list[str] = []

            for x in range(self.WIDTH):
                chars.append('#' if Vec(x, y) in self.tiles else '.')

            lines.append(''.join(chars))

        return '\n'.join(lines)
    
    def translate(self, v: Vec) -> 'Shape':
        return Shape(self.index, frozenset({t + v for t in self.tiles}))

    def hflip(self) -> 'Shape':
        return Shape(self.index, frozenset({
            t.hflip() for t in self.tiles
        })).translate(Vec(Shape.WIDTH - 1, 0))
    
    def rot_clockwise(self) -> 'Shape':
        return Shape(self.index, frozenset({
            t.rot_clockwise() for t in self.tiles
        })).translate(Vec(Shape.WIDTH - 1, 0))
    
    def variants(self) -> list['Shape']:
        return [
            self,
            self.rot_clockwise(),
            self.rot_clockwise().rot_clockwise(),
            self.rot_clockwise().rot_clockwise().rot_clockwise(),
            self.hflip(),
            self.hflip().rot_clockwise(),
            self.hflip().rot_clockwise().rot_clockwise(),
            self.hflip().rot_clockwise().rot_clockwise().rot_clockwise(),
        ]
    
@dataclass(frozen=True, slots=True)
class Region:
    width: int
    height: int
    required_presents: list[int]

    def can_fit_its_shapes(self, shapes: list[Shape]) -> bool:
        print(f'CHECKING IF REGION {self} can fit its shapes')

        board = Board(Rect(Point(0, 0), Point(self.width, self.height)))

        shapes_to_fit: list[Shape] = []

        for shape_index, quantity in enumerate(self.required_presents):
            for _ in range(quantity):
                shapes_to_fit.append(shapes[shape_index])

        print(
            'SHAPES WE NEED TO FIT: '
            + str([shape.index for shape in shapes_to_fit])
        )

        number_of_shapes_to_fit = len(shapes_to_fit)

        if self.width * self.height >= Shape.WIDTH * Shape.HEIGHT * number_of_shapes_to_fit:
            print('ENOUGH TILES TO FIT SHAPES WITHOUT OVERLAPS')
            return True

        if self.width * self.height < sum(len(shape.tiles) for shape in shapes_to_fit):
            print('NOT ENOUGH TILES TO FIT SHAPES EVEN IF THEY ALL OVERLAP PERFECTLY')
            return False

        input()


        boards_after_fit = ways_to_fit(board, tuple(shapes_to_fit))
        return next(boards_after_fit, None) is not None

@dataclass(frozen=True, slots=True)
class Board:
    base: Rect
    shapes: frozenset[tuple[Point, Shape]] = field(default_factory=frozenset)

    def occupier(self, tile: Point) -> Shape | None:
        for loc, shape in self.shapes:
            for t in shape.tiles:
                if loc + t == tile:
                    return shape
                
        return None
    
    def is_occupied(self, tile: Point) -> bool:
        return self.occupier(tile) is not None

    def free_tiles(self) -> Generator[Point]:
        for p in self.base:
            if not self.is_occupied(p):
                yield p

    def can_fit_shape_at(self, loc: Point, variant: Shape) -> bool:
        for t in variant.tiles:
            p = loc + t

            if p not in self.base:
                return False
            if self.is_occupied(p):
                return False
            
        return True
    
    def with_shape(self, loc: Point, variant: Shape) -> 'Board':
        return Board(
            self.base,
            self.shapes | frozenset({(loc, variant)})
        )
    
    def picture(self) -> str:
        def draw(p: Point) -> str:
            occupier = self.occupier(p)

            if occupier is None:
                return '.'
        
            return str(occupier.index)

        return self.base.picture(draw)

def ways_to_fit(board: Board, shapes: tuple[Shape, ...]) -> Generator[Board]:
    # print(f'ways_to_fit({board}, {[shape.index for shape in shapes]})')
    # input()

    if not shapes:
        yield board
        return
    
    first_shape = shapes[0]
    other_shapes = tuple(shapes[1:])
    
    for board_after_fit in ways_to_fit_one_shape(board, first_shape):
        for board_after_all_fits in ways_to_fit(board_after_fit, other_shapes):
            yield board_after_all_fits

@ft.cache
def ways_to_fit_one_shape(board: Board, shape: Shape) -> list[Board]:
    #print(f'ways_to_fit_one_shape({board.base}, {shape.index})')
    result: list[Board] = []

    for variant in shape.variants():
        for loc in board.free_tiles():
            if board.can_fit_shape_at(loc, variant):
                # print(f'can fit shape {shape.index} at {loc}')
                # print(board.with_shape(loc, variant).picture())
                # input()
                result.append(board.with_shape(loc, variant))

    return result

# let's say we decompose the region into two parts, A and B
# then the number of ways to fit a given shape in that region will be:
# the number of ways to fit it into part A
# plus the number of ways to fit it into part B
# plus the number of ways to fit it into the whole region where at least one
#  tile within the shape is in part A and at least one tile is within part B

def parse(ip: str) -> tuple[list[Shape], list[Region]]:
    parts = ip.split('\n\n')

    shape_parts = parts[:-1]
    shapes: list[Shape] = []
    
    for i, shape_part in enumerate(shape_parts):
        shape_lines = shape_part.splitlines()
        index = int(shape_lines[0].strip().strip(':'))
        assert index == i
        shapes.append(Shape.from_lines(index, shape_lines[1:]))

    region_part = parts[-1]
    region_lines = region_part.splitlines()
    regions: list[Region] = []

    for line in region_lines:
        dims, presents_str = (s.strip() for s in line.split(':'))
        width, height = map(int, dims.split('x'))
        presents = map(int, presents_str.split())
        regions.append(Region(width, height, list(presents)))

    return shapes, regions

def first_shape_variant_pics(ip: str) -> str:
    shapes, _ = parse(ip)
    first_shape = shapes[0]
    return '\n\n'.join(v.picture() for v in first_shape.variants())

def p1(ip: str) -> int:
    # need to determine, for each region, whether it can fit the listed shapes
    # in the given quantities

    # so, all the shapes seem to occupy 3 x 3 square regions
    # given a region, we need to be able to determine all the different ways
    # a shape can fit into that region. the region is not necessarily square, it
    # may have chunks taken out of it due to shapes that have already been
    # placed
    shapes, regions = parse(ip)
    return sum(1 for region in regions if region.can_fit_its_shapes(shapes))

    # Since each shape only occupies a 3 x 3 square, we can definitely fit all
    # the shapes if the region can accomodate n 3 x 3 squares where n is the 
    # numeber of shapes to be fitted---and this is the case if the area of
    # the rect >= 9 * n

    # 9 * 6 = 54 = 
    # 9 * 7 = 63


# ok wait
# each shape takes up a certain amount of tiles
# and each rectangle only has a certain amount
# so if the rectangle doesn't have enough tiles they certainly won't fit!
# eg..
# 4 x 4 : 0 0 0 0 2 0  --- shape 4 occupies 7 tiles, hence we need 14 in the
#   rect, which is ok
# 12 x 5 : 1 0 1 0 2 2 -- actually shapes for the examples occupy 7 tiles, so
#   we need 7 * (1 + 1 + 2 + 2) = 7 * 6 = 42 tiles in the rect, which is ok
#    since 12 x 5 = 60
# 12 x 5 : 1 0 1 0 3 2 -- 1 + 1 + 3 + 2 = 7, giving us 49, which is still less
#    than 60. hmm, so this won't work all the time

# 36 x 46 : 46 47 43 46 36 38
# 36 x 46 = 1656
#  46 * 6 + 47 * 7 + 43 * 6 + 46 * 7 + 36 * 7 + 38 * 7 = 1703, so we can rule this one out!
# but since we can't even solve the example atm, this isn't good enoguh

# but we can reason like this:
# if the rects of shape 0 and shape 1 overlap, they
# overlap in at most 2 places
# therefore each combination of shape 0 and shape 1 in the region
# must occupy at least 9 + 9 - 4 = 14 places
# ok that doesn't really help us
#
# but shape 0 with shape 2 --- there is a max of 1 overlap here
# so their combination will occupy 9 + 9 - 2 = 16 places, which is more than 14
# (oh, but given that the shapes can rotate, there can be an overlap of 2)
# 
# given a multiset of shapes, we can try to calculate the minimum number of 
# tiles
# they must occupy
# then we can just apply that to our regions