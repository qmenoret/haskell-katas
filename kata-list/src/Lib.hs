module Lib where

-- pp <=> ++ operator
pp :: [a] -> [a] -> [a]
pp [] a = a
pp a [] = a
pp (x:xs) a = x : pp xs a

-- head
head' :: [a] -> a
head' [] = error "No head in empty list"
head' a  = a !! 0

-- last
last' :: [a] -> a
last' [] = error "No last in empty list"
last' [a] = a
last' (_:xs) = last' xs

-- tail
tail' :: [a] -> [a]
tail' [] = error "No tail in empty list"
tail' (_:xs) = xs

-- init
init' :: [a] -> [a]
init' [] = error "No init in empty list"
init' [a] = []
init' (x:xs) = x : init xs

-- uncons
uncons' :: [a] -> Maybe (a, [a])
uncons' [] = Nothing
uncons'(x:xs) = Just (x, xs)

-- null
null' :: Foldable t => t a -> Bool
null' l = foldl (\acc _ -> False) True l

-- length
length' :: Foldable t => t a -> Int
length' l = foldl (\acc _ -> acc+1) 0 l

-- map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs `pp` [x]

-- intersperse
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [a] = [a]
intersperse' a (x:xs) = x : (a : (intersperse' a xs))

-- intercalate
intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [[]] = []
intercalate' _ [[a]] = [a]
intercalate' [] [a] = a
intercalate' a (x:xs) = (x `pp` a) `pp` intercalate' a xs

-- transpose
transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' l  
    | null tail || null (head' tail) = base
    | otherwise = base `pp` tail
    where base = [[ head' a | a <- l, not (null a) ]]
          tail = transpose' [drop 1 a | a <- l, not (null a)]

-- subsequences
subsequences' :: [a] -> [[a]]
subsequences' l = subsequencesReccur l [] 
    where
        subsequencesReccur [] a = [[]] ++ a 
        subsequencesReccur (x:xs) l = subsequencesReccur xs (l ++ [[x]] ++ [ b ++ [x] | b <- l])

