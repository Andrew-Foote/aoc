{-# LANGUAGE OverloadedStrings #-}
module Y2024.D7 where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text, pack, splitOn, unpack)
import qualified Data.Text as T

import AOC (Sol(..), Test(..))

data Op
    = Add
    | Mul
    | Cat

data Eqn = Eqn
    { eqnTestVal  :: Integer
    , eqnOperands :: NonEmpty Integer
    } deriving Show

parseEqn :: Text -> Eqn
parseEqn eqn = eqn & splitOn ":" & (\case
    [testValS, operandsS] -> Eqn
        { eqnTestVal  = testValS
            & unpack
            & read
        , eqnOperands = operandsS
            & T.words
            <&> unpack
            <&> read
            & NEL.fromList
        }
    _ -> error $ "Invalid equation '" ++ show eqn ++ "'")

parse :: String -> [Eqn]
parse = pack >>> T.lines >>> map parseEqn

digitCount :: Integer -> Integer
digitCount x = floor (logBase 10 $ fromInteger x :: Double) + 1

hasSatisfiableTestVal :: [Op] -> Eqn -> Bool
hasSatisfiableTestVal availOps (Eqn testVal operands) =
    recurse testVal $ NEL.reverse operands
      where
        recurse :: Integer -> NonEmpty Integer -> Bool
        recurse tv (operand :| operands2) = case operands2 of
            operand2:operands3 -> availOps & any (\case
                Add -> recurse (tv - operand) restOperands
                Mul -> case divMod tv operand of
                    (q, r) -> (r == 0) && recurse q restOperands
                Cat -> case divMod tv (10 ^ digitCount operand) of
                    (q, r) -> (r == operand) && recurse q restOperands)
                  where restOperands = operand2 :| operands3
            [] -> operand == tv

sumSatisfiableTestVals :: [Op] -> [Eqn] -> Integer
sumSatisfiableTestVals availOps = 
    filter (hasSatisfiableTestVal availOps)
    >>> map eqnTestVal
    >>> sum

p1 :: String -> String
p1 = parse >>> sumSatisfiableTestVals [Add, Mul] >>> show

p2 :: String -> String
p2 = parse >>> sumSatisfiableTestVals [Add, Mul, Cat] >>> show

sol :: Sol
sol = Sol
    [("p1", p1), ("p2", p2)]
    [
        Test
            "example"
            (unlines
                [ "190: 10 19"
                , "3267: 81 40 27"
                , "83: 17 5"
                , "156: 15 6"
                , "7290: 6 8 6 15"
                , "161011: 16 10 13"
                , "192: 17 8 14"
                , "21037: 9 7 18 13"
                , "292: 11 6 16 20"
                ])

            [("p1", "3749"), ("p2", "11387")]
    ]