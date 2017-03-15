module Lib
    ( diamond
    ) where

import Data.Char

diamond :: Char -> String
diamond 'A' = "A"
diamond 'a' = "A"
diamond c = total
    where
        total = init lines
        lines = (unlines flines) ++ (unlines llines)
        flines = buildLines chars (length chars - 1) (-1)
        llines = reverse (init flines)
        chars = ['A'..(toUpper c)]

buildLines :: String -> Int -> Int -> [String]
buildLines "" _ _ = []
buildLines (c:cs) b i = line c b i :(buildLines cs (b - 1) (i + 2))

line :: Char -> Int -> Int -> String
line c spaceB spaceI = spaces ++ (center spaceI) ++ spaces
    where
        spaces = (replicate spaceB ' ')
        center (-1) = [c]
        center i = [c] ++ (replicate i ' ') ++ [c]
