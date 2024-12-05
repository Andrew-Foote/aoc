module Y2024.D4 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Linear.V2 (V2(..))

import AOC (Sol(..), Test(..))

-- we need to find all substrings matching XMAS,
-- where a substring can be obtained by traversing
-- the grid horizontally, vertically or diagonally,
-- in either direction, and overlaps count.

-- iterate over the wordsearch, whenever we find an
-- X, search in each direction for a match

type Grid = Map (V2 Integer) Char

parse :: String -> Grid
parse ip = lines ip
    & zip [0..]
    <&> \case
        (rowNum, line) -> zip [0..] line
            <&> \case (colNum, letter) -> (V2 colNum rowNum, letter)
    & concat
    & Map.fromList

parseS :: String -> String
parseS ip = show $ parse ip

dirs :: [V2 Integer]
dirs = [
    V2 0 1, -- downwards
    V2 0 (-1), -- upwards
    V2 1 0, -- rightwards
    V2 (-1) 0, -- leftwards
    V2 1 1, -- diagonally to the bottom right
    V2 1 (-1), -- diagonally to the top right
    V2 (-1) 1, -- diagonally to the bottom left
    V2 (-1) (-1) -- diagonally to the top left
    ]

isXmas :: Grid -> V2 Integer -> V2 Integer -> Bool
isXmas grid pos dir = [0..3]
    <&> (\n -> pos + (pure n) * dir)
    <&> (\pathPos -> Map.lookup pathPos grid)
    & (== [Just 'X', Just 'M', Just 'A', Just 'S'])

p1 :: String -> String
p1 ip = let
    grid = parse ip
    in Map.keys grid
        <&> (\pos -> length $ filter (\dir -> isXmas grid pos dir) dirs)
        & sum
        & show

p1Pic :: String -> String
p1Pic ip = "0"

sol :: Sol
sol = Sol
    [
        ("parseS", parseS),
        ("p1", p1),
        ("p1Pic", p1Pic)
    ]
    [
        Test "smallExample"
        (unlines ["MXM", "ABJ"])
        [
            ("parseS", "fromList [(V2 0 0,'M'),(V2 0 1,'A'),(V2 1 0,'X'),(V2 1 1,'B'),(V2 2 0,'M'),(V2 2 1,'J')]")
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
            ])
        ]
    ]