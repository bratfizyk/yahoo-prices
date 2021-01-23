module Web.Data.Yahoo.Time (dayAsEpoch) where

import Data.Int (Int64)
import Data.Time.Calendar (Day, fromGregorian, diffDays)
import Data.Time (UTCTime, defaultTimeLocale, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getCurrentTime, utcTimeToPOSIXSeconds)

secondsSinceEpoch :: UTCTime -> Int64
secondsSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

dayAsEpoch :: Day -> Integer
dayAsEpoch day = 
    3600 * 24 * (diffDays day start)
    where
        start :: Day
        start = fromGregorian 1970 1 1