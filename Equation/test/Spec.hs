import Test.Hspec
import Test.Hspec.QuickCheck

import Lib

main :: IO ()
main = hspec $ do
  describe "Equation test suite" $ do 
    it "Empty equation" $ do
        resolve "x = 0" `shouldBe` 0

    it "Simple eq" $ do
        resolve "x = 1" `shouldBe` 1
        resolve "x = 5" `shouldBe` 5

    it "Negative equation" $ do
        resolve "x = -1" `shouldBe` -1

    it "More than 10" $ do
        resolve "x = 11"  `shouldBe`   11
        resolve "x = -11" `shouldBe` -11

    it "Multiply x" $ do
        resolve "2x = 0" `shouldBe` 0
        resolve "2x = 2" `shouldBe` 1

    it "Multiply and add x" $ do
        resolve "2x + 2 = 0" `shouldBe` -1
        resolve "2x - 2 = 0" `shouldBe`  1
        resolve "2x + 2 = 2" `shouldBe`  0

    it "Multiply and add x -- More complexity" $ do
        resolve "2x + 25 = 12" `shouldBe` -6.5
        resolve "7x - 1  = -8" `shouldBe` -1

    it "Multiplier more than 10" $ do
        resolve "12x = 0"       `shouldBe` 0
        resolve "12x = 12"      `shouldBe` 1
        resolve "12x - 12 = 12" `shouldBe` 2

    it "Multiplier < -1" $ do
        resolve "-2x = 0" `shouldBe`  0
        resolve "-2x = 2" `shouldBe` -1

    it "Multiplier = -1" $ do
        resolve "-x = 0" `shouldBe`  0
        resolve "-x = 1" `shouldBe` -1
