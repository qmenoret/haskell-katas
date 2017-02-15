
import Test.Hspec
import Control.Exception (evaluate)

import Lib
import Data.List

main = hspec $ do
  describe "the list lib" $ do
    it "++" $ do
        pp [] [] `shouldBe` ([] :: [Int])
        pp [1,2,3] [] `shouldBe` [1,2,3]
        pp [] [4,5,6] `shouldBe` [4,5,6]
        pp [1,2,3] [4,5,6] `shouldBe` [1,2,3,4,5,6]
        pp [1,2,3] [5,6] `shouldBe` [1,2,3,5,6]
        pp [1,2] [4,5,6] `shouldBe` [1,2,4,5,6]
    it "head" $ do
        head' [1] `shouldBe` 1
        head' [0,2] `shouldBe` 0
        head' [] `shouldThrow` anyException
    it "last" $ do
        last' [1] `shouldBe` 1
        last' [0,2] `shouldBe` 2
        last' [] `shouldThrow` anyException
    it "tail" $ do
        tail' [1] `shouldBe` []
        tail' [1,2,3,4] `shouldBe` [2,3,4]
        evaluate (tail' []) `shouldThrow` anyException
    it "init" $ do
        init' [1] `shouldBe` []
        init' [1,2,3,4] `shouldBe` [1,2,3]
        evaluate (init' []) `shouldThrow` anyException
    it "uncons" $ do
        uncons' ([] :: [Int]) `shouldBe` Nothing
        uncons' [1] `shouldBe` Just (1, [])
        uncons' [1,2,3] `shouldBe` Just (1, [2,3])
    it "null" $ do
        null' [] `shouldBe` True
        null' [0..3] `shouldBe` False
    it "length" $ do
        length' [] `shouldBe` 0
        length' [0..2] `shouldBe` 3
    it "map" $ do
        map' (max 2) [] `shouldBe` []
        map' (max 2) [1] `shouldBe` [2]
        map' (max 2) [1,2,3] `shouldBe` [2,2,3]
    it "reverse" $ do
        reverse' [] `shouldBe` ([] :: [Int])
        reverse' [1] `shouldBe` [1]
        reverse' [1..10] `shouldBe` [10,9..1]
    it "intersperse" $ do
        intersperse' 0 [] `shouldBe` []
        intersperse' 0 [1] `shouldBe` [1]
        intersperse' 0 [1,2] `shouldBe` [1,0,2]
        intersperse' 0 [1,2,3,4] `shouldBe` [1,0,2,0,3,0,4]
    it "intercalate" $ do
        intercalate' [0] [[]] `shouldBe` []
        intercalate' [0] [[1]] `shouldBe` [1]
        intercalate' [0] [[1],[2]] `shouldBe` [1,0,2]
        intercalate' [0] [[1],[2],[3],[4]] `shouldBe` [1,0,2,0,3,0,4]
    it "transpose" $ do
        transpose' [[],[]] `shouldBe` ([[]] :: [[Int]])
        transpose' [[1,2,3],[4,5,6]] `shouldBe` [[1,4],[2,5],[3,6]]
        transpose' [[10,11],[20],[],[30,31,32]] `shouldBe` [[10,20,30],[11,31],[32]]
    it "subsequences" $ do
        subsequences' "" `shouldBe` [""]
        subsequences' "a" `shouldBe` ["", "a"]
        subsequences' "ab" `shouldBe` ["", "a", "b", "ab"]
        subsequences' "abc" `shouldBe` ["","a","b","ab","c","ac","bc","abc"]
    -- it "permutations" $ do
    --    permutations' ""  `shouldBe` [""] 
    --    permutations' "a" `shouldBe` ["a"] 
    --    permutations' "ab" `shouldBe` ["ab","ba"] 
    --    permutations' "abc" `shouldBe` ["abc","bac","cba","bca","cab","acb"]


