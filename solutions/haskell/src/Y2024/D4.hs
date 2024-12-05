module Y2024.D4 where

import Control.Lens.Getter ((^.))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Linear.V2 (V2(..), _x, _y)

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

xmasPath :: Grid -> V2 Integer -> V2 Integer -> Maybe [V2 Integer]
xmasPath grid pos dir = let
    path = [0..3] <&> \n -> let
        pathPos = pos + (pure n) * dir
        in (pathPos, Map.lookup pathPos grid)
    in case map snd path of
        [Just 'X', Just 'M', Just 'A', Just 'S'] -> Just $ map fst path
        _ -> Nothing

isXmas :: Grid -> V2 Integer -> V2 Integer -> Bool
isXmas grid pos dir = isJust $ xmasPath grid pos dir

p1 :: String -> String
p1 ip = let
    grid = parse ip
    in Map.keys grid
        <&> (\pos -> length $ filter (isXmas grid pos) dirs)
        & sum
        & show

width :: Grid -> Integer
width grid = Map.keys grid
    <&> (^. _x)
    & maximum
    & (+1)

height :: Grid -> Integer
height grid = Map.keys grid
    <&> (^. _y)
    & maximum
    & (+1)

drawGrid :: Grid -> String
drawGrid grid = [0..(height grid - 1)]
    <&> (\y -> [0..(width grid - 1)]
            <&> (\x -> case Map.lookup (V2 x y) grid of
                Just c -> c
                Nothing -> '.'))
    & unlines

p1Pic :: String -> String
p1Pic ip = let
    grid = parse ip
    keysToKeep = Map.keys grid
        <&> (\pos -> dirs
            <&> (\dir -> case xmasPath grid pos dir of
                Just path -> Set.fromList path
                Nothing -> Set.empty)
            & Set.unions)
        & Set.unions
    in grid
        & Map.filterWithKey (\key _ -> Set.member key keysToKeep)
        & drawGrid

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