module Web.Data.Yahoo.Time (dayAsEpoch) where


secondsSinceEpoch :: UTCTime -> Int64
secondsSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

dayAsEpoch :: Day -> Integer
dayAsEpoch day = 
    3600 * 24 * (diffDays day start)
    where
        start :: Day
        start = fromGregorian 1970 1 1