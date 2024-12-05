module Main where

import Data.Aeson ( encode )
import Data.Maybe ( isJust )
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import System.Environment ( getArgs )
import System.Exit ( die )

import AOC ( Sol(solTests, solFacets) )
import qualified Y2015.D5
import qualified Y2024.D1
import qualified Y2024.D4

sol :: Integer -> Integer -> Sol
sol year day = case (year, day) of
    (2015, 5) -> Y2015.D5.sol
    (2024, 1) -> Y2024.D1.sol
    (2024, 4) -> Y2024.D4.sol
    _ -> error $ "Missing solution map entry for "
        ++ show year ++ " day " ++ show day

hasFacet :: Integer -> Integer -> String -> IO ()
hasFacet year day facet = do
    let ans = isJust $ lookup facet $ solFacets $ sol year day
    putStr (if ans then "y" else "n")

runFacet :: Integer -> Integer -> String -> IO ()
runFacet year day facet = do
    let mFacet = lookup facet $ solFacets $ sol year day
    case mFacet of
        Just f -> do
            input <- getContents
            putStr $ f input
        Nothing -> die $ "facet '" ++ facet ++ "' does not exist"

testDefs :: Integer -> Integer -> IO ()
testDefs year day = putStrLn $
    Text.unpack $ Text.decodeUtf8 $ encode $ solTests $ sol year day

main :: IO ()
main = do
    args <- getArgs
    case args of
        yearS:dayS:mode:args2 -> do
            let year = read yearS :: Integer
            let day  = read dayS :: Integer
            case [mode] ++ args2 of
                ["has_facet", facet] -> hasFacet year day facet
                ["run_facet", facet] -> runFacet year day facet
                ["test_defs"]        -> testDefs year day
                _ -> die ("Unrecognised arguments format: "
                    ++ unwords ([yearS, dayS, mode] ++ args2))
        _ -> die "No argument supplied"
