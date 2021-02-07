{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Lazy (ByteString)
import Data.Either (isRight)
import Test.Hspec (hspec, describe, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)

import Gen ()
import Web.Data.Yahoo.API (fetchLatest)
import Web.Data.Yahoo.Utils (dayAsEpoch)
import Web.Data.Yahoo.Response (PriceResponse, tryParseAsPrice)

isDivisibleBy :: Integral a => a -> a -> Bool
isDivisibleBy n x = (x `mod` n) == 0

sample :: ByteString
sample =
    "Date,Open,High,Low,Close,Adj Close,Volume\n\
    \2020-02-07,24.620001,24.740000,24,469999,24.600000,23.815666,7072600"

main :: IO ()
main = hspec $ do
    describe "Utils" $ do
        prop "epoch day is divisible by 86400" $
            \d -> dayAsEpoch d `shouldSatisfy` isDivisibleBy 86400

    describe "Response" $ do
        it "parses XML schema" $
            tryParseAsPrice sample `shouldSatisfy` isRight

    describe "Yahoo API" $ do
        it "connects remote API" $ do
            resp <- fetchLatest "RSX"
            resp `shouldSatisfy` isRight