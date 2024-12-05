module Y2024.D1 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate, sort)

import AOC (Sol(..), Test(..))

parse :: String -> ([Integer], [Integer])
parse ip =
    lines ip
    <&> words
    <&> \case
        [l, r] -> (read l :: Integer, read r :: Integer)
        _ -> error "expected exactly two words per line"
    & unzip

lists :: String -> ([Integer], [Integer])
lists ip = case parse ip of
    (l, r) -> (sort l, sort r)

listsS :: String -> String
listsS ip = case lists ip of
    (l, r) -> intercalate ";" [
        intercalate "," $ map show l,
        intercalate "," $ map show r
        ]

dists :: String -> [Integer]
dists ip = case lists ip of
    (l, r) -> map abs $ zipWith (-) l r

distsS :: String -> String
distsS ip = intercalate "," $ map show $ dists ip

p1 :: String -> String
p1 ip = show $ sum $ dists ip

sol = Sol
    [("listsS", listsS), ("distsS", distsS), ("p1", p1)]
    [
        Test "example"
        (unlines [
            "3   4",
            "4   3",
            "2   5",
            "1   3",
            "3   9",
            "3   3"
        ])
        [
            ("listsS", "1,2,3,3,3,4;3,3,3,4,5,9"),
            ("distsS", "2,1,0,1,2,5"),
            ("p1", "11")
        ]
    ]