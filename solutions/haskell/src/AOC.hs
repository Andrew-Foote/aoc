module AOC where

import Data.Aeson ( ToJSON(toJSON) )

data Test = Test {
    testInputName :: String,
    testInput     :: String,
    testFacets    :: [(String, String)]
}

instance ToJSON Test where
    toJSON (Test inputName input facets) = toJSON (inputName, input, facets)

data Sol = Sol {
    solFacets :: [(String, String -> String)],
    solTests :: [Test]
}