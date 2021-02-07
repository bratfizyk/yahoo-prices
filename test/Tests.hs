module Main (main) where

import Data.Either (isRight)
import Test.Hspec (hspec, describe, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)

import Gen ()
import Web.Data.Yahoo.API (fetchLatest)
import Web.Data.Yahoo.Utils (dayAsEpoch)

isDivisibleBy :: Integral a => a -> a -> Bool
isDivisibleBy n x = (x `mod` n) == 0

main :: IO ()
main = hspec $ do
    describe "Utils" $ do
        prop "epoch day is divisible by 86400" $
            \d -> dayAsEpoch d `shouldSatisfy` isDivisibleBy 86400


    describe "Yahoo API" $ do
        it "connects remote API" $ do
            resp <- fetchLatest "RSX"
            resp `shouldSatisfy` isRight