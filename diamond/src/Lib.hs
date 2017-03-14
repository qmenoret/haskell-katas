module Lib
    ( diamond
    ) where

import Data.Char

diamond :: Char -> String
diamond 'A' = "A"
diamond 'a' = "A"
diamond z = total
    where
        total = first ++ '\n':lines ++ first
        lines = (unlines flines) ++ (unlines llines)
        flines = buildLines xs (length xs - 1) 1
        llines = reverse (init flines)
        first = (replicate (length xs) ' ') ++ "A" ++ (replicate (length xs) ' ')
        (c:xs) = ['A'..(toUpper z)]
         
buildLines :: String -> Int -> Int -> [String]
buildLines "" _ _ = []
buildLines (c:cs) b i = line c b i :(buildLines cs (b - 1) (i + 2))

line :: Char -> Int -> Int -> String
line c spaceB spaceI = (replicate spaceB ' ') ++ [c] ++ (replicate spaceI ' ') ++ [c] ++ (replicate spaceB ' ')
