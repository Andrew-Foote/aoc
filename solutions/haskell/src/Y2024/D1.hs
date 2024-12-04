import Control.Monad
import System.Environment
import System.Exit

p1 :: String -> String
p1 _ = "wshj"

p2 :: String -> String
p2 _ = "193i3"

hasFacetMain :: String -> IO ()
hasFacetMain facet = do
    when (elem facet ["p1", "p2"]) $ putStrLn "y"

runFacetMain :: String -> IO ()
runFacetMain facet = do
    input <- getContents
    f <- pure $ case facet of
        "p1" -> p1
        "p2" -> p2
    putStrLn $ f input

testDefsMain :: IO ()
testDefsMain = do
    putStrLn "not implemented"

main = do
    args <- getArgs
    case args of
        "-e":[facet] -> hasFacetMain facet
        "-f":[facet] -> runFacetMain facet
        "-t":[facet] -> testDefsMain
        _            -> die "Bad args"

-- main = do
--     putStrLn "ARGS"
--     getArgs >>= traverse_ putStrLn
--     putStrLn "INPUT"
--     getContents >>= putStrLn