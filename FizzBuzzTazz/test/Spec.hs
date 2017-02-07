
import Test.Hspec
import Test.Hspec.QuickCheck

import Lib

main :: IO ()
main = hspec $ do
  describe "FizzBuzzTazz test suite" $ do 
    it "No match" $ do
        fizzBuzzTazz 0 `shouldBe` ""
        fizzBuzzTazz 1 `shouldBe` ""
        fizzBuzzTazz 2 `shouldBe` ""
        fizzBuzzTazz 4 `shouldBe` ""

    it "Fizz" $ do
        fizzBuzzTazz 3 `shouldBe` "Fizz"
        fizzBuzzTazz 6 `shouldBe` "Fizz"
        
    it "Buzz" $ do
        fizzBuzzTazz 5  `shouldBe` "Buzz"
        fizzBuzzTazz 10 `shouldBe` "Buzz"
        
    it "Tazz" $ do
        fizzBuzzTazz 7  `shouldBe` "Tazz"
        fizzBuzzTazz 14 `shouldBe` "Tazz"
        
    it "FizzBuzz" $ do
        fizzBuzzTazz 15 `shouldBe` "FizzBuzz"
        fizzBuzzTazz 30 `shouldBe` "FizzBuzz"
        
    it "FizzTazz" $ do
        fizzBuzzTazz 21 `shouldBe` "FizzTazz"
        fizzBuzzTazz 42 `shouldBe` "FizzTazz"
        
    it "BuzzTazz" $ do
        fizzBuzzTazz 35 `shouldBe` "BuzzTazz"
        fizzBuzzTazz 70 `shouldBe` "BuzzTazz"
        
    it "FizzBuzzTazz" $ do
        fizzBuzzTazz 105 `shouldBe` "FizzBuzzTazz"
        fizzBuzzTazz 210 `shouldBe` "FizzBuzzTazz"
        
  describe "FizzBuzzTazz test suite (part 2)" $ do 

    it "Mozz" $ do
        fizzBuzzTazz 11 `shouldBe` "Mozz"
        fizzBuzzTazz 22 `shouldBe` "Mozz"
        
    it "FizzMozz" $ do
        fizzBuzzTazz 33 `shouldBe` "FizzMozz"
        fizzBuzzTazz 66 `shouldBe` "FizzMozz"
        
    it "BuzzMozz" $ do
        fizzBuzzTazz 55  `shouldBe` "BuzzMozz"
        fizzBuzzTazz 110 `shouldBe` "BuzzMozz"
        
    it "TazzMozz" $ do
        fizzBuzzTazz 77  `shouldBe` "TazzMozz"
        fizzBuzzTazz 154 `shouldBe` "TazzMozz"
        
    it "FizzBuzzMozz" $ do
        fizzBuzzTazz 165 `shouldBe` "FizzBuzzMozz"
        fizzBuzzTazz 330 `shouldBe` "FizzBuzzMozz"
        
    it "FizzTazzMozz" $ do
        fizzBuzzTazz 231 `shouldBe` "FizzTazzMozz"
        fizzBuzzTazz 462 `shouldBe` "FizzTazzMozz"
        
    it "BuzzTazzMozz" $ do
        fizzBuzzTazz 385 `shouldBe` "BuzzTazzMozz"
        fizzBuzzTazz 770 `shouldBe` "BuzzTazzMozz"
        
    it "FizzBuzzTazzMozz" $ do
        fizzBuzzTazz 1155 `shouldBe` "FizzBuzzTazzMozz"
        fizzBuzzTazz 2310 `shouldBe` "FizzBuzzTazzMozz"

  describe "FizzBuzzTazz test suite (part 3)" $ do 
        
    it "Vezz" $ do
        fizzBuzzTazz 13 `shouldBe` "Vezz"
        fizzBuzzTazz 26 `shouldBe` "Vezz"
        
    it "FizzVezz" $ do
        fizzBuzzTazz 39 `shouldBe` "FizzVezz"
        fizzBuzzTazz 78 `shouldBe` "FizzVezz"
        
    it "BuzzVezz" $ do
        fizzBuzzTazz 65  `shouldBe` "BuzzVezz"
        fizzBuzzTazz 130 `shouldBe` "BuzzVezz"
        
    it "TazzVezz" $ do
        fizzBuzzTazz 91  `shouldBe` "TazzVezz"
        fizzBuzzTazz 182 `shouldBe` "TazzVezz"
        
    it "MozzVezz" $ do
        fizzBuzzTazz 143 `shouldBe` "MozzVezz"
        fizzBuzzTazz 286 `shouldBe` "MozzVezz"
        
    it "FizzBuzzVezz" $ do
        fizzBuzzTazz 195 `shouldBe` "FizzBuzzVezz"
        fizzBuzzTazz 390 `shouldBe` "FizzBuzzVezz"
        
    it "FizzTazzVezz" $ do
        fizzBuzzTazz 273 `shouldBe` "FizzTazzVezz"
        fizzBuzzTazz 546 `shouldBe` "FizzTazzVezz"
        
    it "FizzMozzVezz" $ do
        fizzBuzzTazz 429 `shouldBe` "FizzMozzVezz"
        fizzBuzzTazz 858 `shouldBe` "FizzMozzVezz"
        
    it "BuzzTazzVezz" $ do
        fizzBuzzTazz 455 `shouldBe` "BuzzTazzVezz"
        fizzBuzzTazz 910 `shouldBe` "BuzzTazzVezz"
        
    it "BuzzMozzVezz" $ do
        fizzBuzzTazz 715  `shouldBe` "BuzzMozzVezz"
        fizzBuzzTazz 1430 `shouldBe` "BuzzMozzVezz"
        
    it "TazzMozzVezz" $ do
        fizzBuzzTazz 1001 `shouldBe` "TazzMozzVezz"
        fizzBuzzTazz 2002 `shouldBe` "TazzMozzVezz"
        
    it "FizzBuzzTazzVezz" $ do
        fizzBuzzTazz 1365 `shouldBe` "FizzBuzzTazzVezz"
        fizzBuzzTazz 2730 `shouldBe` "FizzBuzzTazzVezz"
        
    it "FizzBuzzMozzVezz" $ do
        fizzBuzzTazz 2145  `shouldBe` "FizzBuzzMozzVezz"
        fizzBuzzTazz 4290 `shouldBe` "FizzBuzzMozzVezz"
        
    it "FizzTazzMozzVezz" $ do
        fizzBuzzTazz 3003 `shouldBe` "FizzTazzMozzVezz"
        fizzBuzzTazz 6006 `shouldBe` "FizzTazzMozzVezz"
        
    it "BuzzTazzMozzVezz" $ do
        fizzBuzzTazz 5005  `shouldBe` "BuzzTazzMozzVezz"
        fizzBuzzTazz 10010 `shouldBe` "BuzzTazzMozzVezz"
        
    it "FizzBuzzTazzMozzVezz" $ do
        fizzBuzzTazz 15015 `shouldBe` "FizzBuzzTazzMozzVezz"
        fizzBuzzTazz 30030 `shouldBe` "FizzBuzzTazzMozzVezz"
