module Lib
    ( fizzBuzzTazz
    ) where

fizzBuzzTazz' :: Int -> String
fizzBuzzTazz' 0 = ""
fizzBuzzTazz' x = (fizz x) ++ (buzz x) ++ (tazz x)
    where
        fizz n = if n `mod` 3 == 0 then "Fizz" else ""
        buzz n = if n `mod` 5 == 0 then "Buzz" else ""
        tazz n = if n `mod` 7 == 0 then "Tazz" else ""

fizzBuzzTazz :: Int -> String
fizzBuzzTazz 0 = ""
fizzBuzzTazz x = foldl (\acc l -> acc ++ (match l)) "" list
    where
        match couple = if (x `mod` (fst couple) == 0) then (snd couple) else ""
        list  = 
            [(3,    "Fizz")
            ,(5,    "Buzz")
            ,(7,    "Tazz")
            ,(11,   "Mozz")
            ,(13,   "Vezz")]
