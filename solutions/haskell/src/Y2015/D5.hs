module Y2015.D5 where

import Data.List (isInfixOf)
import AOC (Sol(..), Test(..))

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

vowelCount :: String -> Int
vowelCount s = length $ filter isVowel s

containsDoubleLetter :: String -> Bool
containsDoubleLetter s = not . null $
    filter (uncurry (==)) $ zip s (drop 1 s)
    
isNice :: String -> Bool
isNice s =
    vowelCount s >= 3
    && containsDoubleLetter s
    && not (any (\ss -> isInfixOf ss s) ["ab", "cd", "pq", "xy"])

isNiceS :: String -> String
isNiceS = show . isNice

p1 :: String -> String
p1 ip = show $ length $ filter isNice $ lines ip

sol :: Sol
sol = Sol
    [("isNiceS", isNiceS), ("p1", p1)]
    [
        Test "example1" "ugknbfddgicrmopn" [("isNiceS", show True)],
        Test "example2" "aaa" [("isNiceS", show True)],
        Test "example3" "jchzalrnumimnmhp" [("isNiceS", show False)],
        Test "example4" "haegwjzuvuyypxyu" [("isNiceS", show False)],
        Test "example5" "dvszwmarrgswjxmb" [("isNiceS", show False)],
        Test
            "allExamples"
            (unlines
                [ "ugknbfddgicrmopn"
                , "aaa"
                , "jchzalrnumimnmhp"
                , "haegwjzuvuyypxyu"
                , "dvszwmarrgswjxmb"
                ])
            [("p1", "2")]
    ]