module Y2024.D1 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate, sort)
import Data.Map (Map)
import qualified Data.Map as Map

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

counter :: [Integer] -> Map Integer Integer
counter nums = foldr
    (\num nums0 -> Map.insertWith (+) num 1 nums0)
    Map.empty
    nums

counterS :: String -> String
counterS ip = case parse ip of
    (_, r) -> show $ Map.toList $ counter r

similarityScores :: String -> [Integer]
similarityScores ip = case parse ip of
    (l, r) -> let
        rCounter = counter r
        in l <&> \num -> num * case Map.lookup num rCounter of
            Just count -> count
            Nothing -> 0

similarityScoresS :: String -> String
similarityScoresS ip = intercalate "," $ map show $ similarityScores ip

p2 :: String -> String
p2 ip = show $ sum $ similarityScores ip

sol :: Sol
sol = Sol
    [
        ("listsS", listsS),
        ("distsS", distsS),
        ("p1", p1),
        ("counterS", counterS),
        ("similarityScoresS", similarityScoresS),
        ("p2", p2)
    ]
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
            ("p1", "11"),
            ("counterS", "[(3,3),(4,1),(5,1),(9,1)]"),
            ("similarityScoresS", "9,4,0,0,9,9"),
            ("p2", "31")
        ]
    ]