module Lib where

type Result = Float
type Equation = String

-- Resolve a first degree simple formated equation
resolve :: Equation -> Result
resolve e = rightV / xmult
    where
        (m, l, r) = splitEq e
        rightV = calculateRight l r
        xmult | null m    = 1                          
              | otherwise = (read . sanitize) m

-- Split an equation between multiplier, left sum and right sum
splitEq :: Equation -> (String, String, String)
splitEq equation = (multiplier, leftsum, rightsum)
    where
        (left, y:rightsum)      = span (/= '=') equation
        (multiplier, x:leftsum) = span (/= 'x') left

-- Calculate real right member by substracting left sum to right value
calculateRight :: String -> String -> Result
calculateRight lPart rPart = (read rPart) - toTranslate
    where
        sanitized = sanitize lPart
        toTranslate = if null sanitized then 0 else read sanitized

-- Remove space and + to get a number read can parse. If "-", return "-1".
sanitize :: String -> String
sanitize = escapeMinus . (filter (`notElem` " +"))
    where 
        escapeMinus "-" = "-1"
        escapeMinus x   = x
