module Gen where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Data.Time.Calendar (Day, gregorianMonthLength, fromGregorian)

instance Arbitrary Day where
    arbitrary = do
        year <- choose (1990, 2020)
        month <- choose (1, 12)
        day <- choose (1, gregorianMonthLength year month)
        return $ fromGregorian year month day