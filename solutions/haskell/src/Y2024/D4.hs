module Y2024.D4 where

import qualified Data.List.NonEmpty as NEList
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear.V2 (V2(..))

import AOC (Sol(..), Test(..))

data Grid = Grid
    { gridWidth :: Integer
    , gridHeight :: Integer
    , gridContent :: Map (V2 Integer) Char
    } deriving Show

gridPoints :: Grid -> [V2 Integer]
gridPoints (Grid _ _ gridContent) = Map.keys gridContent

gridLookup :: V2 Integer -> Grid -> Maybe Char
gridLookup point (Grid _ _ gridContent) = Map.lookup point gridContent

parse :: String -> Grid
parse ip = Grid width height content
  where
    inputLines = NEList.fromList $ lines ip
    height = toInteger $ length inputLines
    width = toInteger $ length $ NEList.head inputLines
    content = 
        zip [0..] (NEList.toList inputLines)
        <&> \case
            (rowNum, line) -> 
                zip [0..] line
                <&> \case
                    (colNum, letter) -> (V2 colNum rowNum, letter)
        & concat
        & Map.fromList

parseS :: String -> String
parseS ip = show $ parse ip

dirs :: [V2 Integer]
dirs = map (uncurry V2)
    [ (-1, -1), (0, -1), (1, -1)
    , (-1,  0),          (1,  0)
    , (-1,  1), (0,  1), (1,  1)
    ]

xmasSet :: Grid -> V2 Integer -> V2 Integer -> Set (V2 Integer)
xmasSet grid pos dir =
    case letters of
        "XMAS" -> Set.fromList path
        _      -> Set.empty
      where
        path = [0..3] <&> (\n -> pos + (pure n) * dir)
        letters = path <&> flip gridLookup grid & catMaybes

isXmas :: Grid -> V2 Integer -> V2 Integer -> Bool
isXmas grid pos dir = not $ Set.null $ xmasSet grid pos dir

p1 :: String -> String
p1 ip =
    gridPoints grid
    <&> (\pos -> length $ filter (isXmas grid pos) dirs)
    & sum
    & show
  where grid = parse ip

drawGrid :: Grid -> String
drawGrid grid =
    [0..(gridHeight grid - 1)]
    <&> (\y ->
        [0..(gridWidth grid - 1)]
        <&> (\x ->
                case gridLookup (V2 x y) grid of
                    Just letter -> letter
                    Nothing     -> '.'))
    & unlines

p1Pic :: String -> String
p1Pic ip = case grid of
    (Grid width height content) ->
        drawGrid $ Grid width height filteredContent
      where
        keysToKeep =
            gridPoints grid
            <&> (\pos -> dirs <&> xmasSet grid pos & Set.unions)
            & Set.unions
        filteredContent = Map.filterWithKey
            (\key _ -> Set.member key keysToKeep)
            content
  where grid = parse ip

cross :: [V2 Integer]
cross = map (uncurry V2)
    [ (-1, -1),         (1, -1)
    ,           (0, 0)
    , (-1,  1),         (1,  1)
    ]

crossMasSet :: Grid -> V2 Integer -> String -> Set (V2 Integer)
crossMasSet grid pos layout =
    if catMaybes letters == layout
        then Set.fromList path
        else Set.empty
      where
        path = cross <&> (+pos)
        letters = path <&> flip gridLookup grid

isCrossMas :: Grid -> V2 Integer -> String -> Bool
isCrossMas grid pos layout = not $ Set.null $ crossMasSet grid pos layout

-- M M  S M  M S  S S
--  A    A    A    A
-- S S  S M  M S  M M
crossMasLayouts :: [String]
crossMasLayouts = ["MMASS", "SMASM", "MSAMS", "SSAMM"]

p2 :: String -> String
p2 ip =
    gridPoints grid
    <&> (\pos -> length $ filter (isCrossMas grid pos) crossMasLayouts)
    & sum
    & show
  where grid = parse ip

p2Pic :: String -> String
p2Pic ip = case grid of
    (Grid width height content) ->
        drawGrid $ Grid width height filteredContent
      where
        keysToKeep =
            gridPoints grid
            <&> (\pos ->
                crossMasLayouts
                <&> crossMasSet grid pos
                & Set.unions)
            & Set.unions
        filteredContent = Map.filterWithKey
            (\key _ -> Set.member key keysToKeep)
            content
  where grid = parse ip

sol :: Sol
sol = Sol
    [
        ("parseS", parseS),
        ("p1", p1),
        ("p1Pic", p1Pic),
        ("p2", p2),
        ("p2Pic", p2Pic)
    ]
    [
        Test "smallExample"
        (unlines ["MXM", "ABJ"])
        [
            ("parseS", show $ Grid 3 2 $ Map.fromList
                [ (V2 0 0, 'M'), (V2 0 1, 'A')
                , (V2 1 0, 'X'), (V2 1 1, 'B')
                , (V2 2 0, 'M'), (V2 2 1, 'J')
                ])
        ],
        Test "example"
        (unlines [
            "MMMSXXMASM",
            "MSAMXMSMSA",
            "AMXSXMAAMM",
            "MSAMASMSMX",
            "XMASAMXAMM",
            "XXAMMXXAMA",
            "SMSMSASXSS",
            "SAXAMASAAA",
            "MAMMMXMMMM",
            "MXMXAXMASX"
        ])
        [
            ("p1", "18"),
            ("p1Pic", unlines [
                "....XXMAS.",
                ".SAMXMS...",
                "...S..A...",
                "..A.A.MS.X",
                "XMASAMX.MM",
                "X.....XA.A",
                "S.S.S.S.SS",
                ".A.A.A.A.A",
                "..M.M.M.MM",
                ".X.X.XMASX"
            ]),
            ("p2", "9"),
            ("p2Pic", unlines [
                ".M.S......",
                "..A..MSMS.",
                ".M.S.MAA..",
                "..A.ASMSM.",
                ".M.S.M....",
                "..........",
                "S.S.S.S.S.",
                ".A.A.A.A..",
                "M.M.M.M.M.",
                ".........."
            ])
        ]
    ]