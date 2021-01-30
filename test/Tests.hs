module Main (main) where

import Data.Either (isRight)
import Web.Data.Yahoo.API
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Yahoo API" $ do
        it "works fine" $
            13 `shouldBe` 13

        it "connects remote API" $ do
            resp <- fetchLatest "RSX"
            resp `shouldSatisfy` isRight