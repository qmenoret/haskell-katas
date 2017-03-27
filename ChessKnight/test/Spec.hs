
import Test.Hspec
import Test.Hspec.QuickCheck

import Lib

main :: IO ()
main = hspec $ do
    describe "ChessKnight test suite" $ do
        it "Zero move" $ do
            Knight (0, 0) `shouldBe` Knight (0, 0)
            [Knight (0, 0)] `moves` 0
                `shouldBe` [Knight (0, 0)]
        it "One move centered" $ do
            [Knight (5, 5)] `moves` 1
                `shouldBe`  [ Knight (3, 4)
                            , Knight (3, 6)
                            , Knight (4, 3)
                            , Knight (4, 7)
                            , Knight (6, 3)
                            , Knight (6, 7)
                            , Knight (7, 4)
                            , Knight (7, 6)
                            ]

        it "One move on side" $ do
            [Knight (0, 0)] `moves` 1
                `shouldBe`  [ Knight (1, 2)
                            , Knight (2, 1)
                            ]

        it "Two move on side" $ do
            [Knight (0, 0)] `moves` 2
                `shouldBe`  [ Knight (0, 0)
                            , Knight (0, 2)
                            , Knight (0, 4)
                            , Knight (1, 3)
                            , Knight (2, 0)
                            , Knight (2, 4)
                            , Knight (3, 1)
                            , Knight (3, 3)
                            , Knight (4, 0)
                            , Knight (4, 2)
                            ]
