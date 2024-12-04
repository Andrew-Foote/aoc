module Main where

import Data.Foldable
import System.Environment

main :: IO ()
main = do
    putStrLn "ARGS"
    getArgs >>= traverse_ putStrLn
    putStrLn "INPUT"
    getContents >>= putStrLn