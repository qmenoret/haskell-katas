import Test.Hspec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Diamond test suite" $ do
        it "Basics" $ do
            diamond 'A' `shouldBe` "A"
            diamond 'a' `shouldBe` "A"
            
        it "Two floors" $ do
            diamond 'B' `shouldBe` " A \nB B\n A "
            diamond 'b' `shouldBe` " A \nB B\n A "
            
        it "Three floors" $ do
            diamond 'C' `shouldBe` "  A  \n B B \nC   C\n B B \n  A  "
            diamond 'c' `shouldBe` "  A  \n B B \nC   C\n B B \n  A  "
            
        it "Four floors" $ do
            diamond 'D' `shouldBe` "   A   \n  B B  \n C   C \nD     D\n C   C \n  B B  \n   A   "
            diamond 'd' `shouldBe` "   A   \n  B B  \n C   C \nD     D\n C   C \n  B B  \n   A   "
