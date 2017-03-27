module Lib
    where

import Control.Monad
import Data.List.Unique

newtype Knight = Knight {knightPos :: (Int, Int)}
    deriving (Eq, Show, Ord)

moves :: [Knight] -> Int -> [Knight]
moves ks 0 = ks
moves ks n = sortUniq $ moves (ks >>= moveKnights) (n-1)

-- moveKnights :: Knight -> [Knight]
-- moveKnights (Knight (x, y)) = filter (\ (Knight (a,b)) -> a >= 0 && b >= 0 && a < 8 && b < 8)
--                                 [ Knight (x -1, y -2)
--                                 , Knight (x -1, y +2)
--                                 , Knight (x +1, y -2)
--                                 , Knight (x +1, y +2)
--                                 , Knight (x -2, y -1)
--                                 , Knight (x -2, y +1)
--                                 , Knight (x +2, y -1)
--                                 , Knight (x +2, y +1)
--                                 ]

moveKnights :: Knight -> [Knight]
moveKnights (Knight (x, y)) = do
    (x', y') <- [ (x -1, y -2), (x -1, y +2)
                , (x +1, y -2), (x +1, y +2)
                , (x -2, y -1), (x -2, y +1)
                , (x +2, y -1), (x +2, y +1)
                ]
    guard (x' >= 0 && y' >= 0 && x' < 8 && y' < 8)
    return (Knight (x', y'))
