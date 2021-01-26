module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "absolute" $ do
        it "works fine" $
            13 `shouldBe` 13