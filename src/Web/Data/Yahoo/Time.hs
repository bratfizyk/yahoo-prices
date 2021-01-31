module Web.Data.Yahoo.Time (dayAsEpoch, right) where

import Data.Time.Calendar (Day, fromGregorian, diffDays)

dayAsEpoch :: Day -> Integer
dayAsEpoch day = 
    3600 * 24 * (diffDays day start)
    where
        start :: Day
        start = fromGregorian 1970 1 1

right :: (t -> b) -> Either a t -> Either a b
right f (Right x) = Right (f x)
right _ (Left x)  = Left x