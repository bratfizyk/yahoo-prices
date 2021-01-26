{-# LANGUAGE OverloadedStrings #-}

module Web.Data.Yahoo.API where
    
import Control.Lens ((^.))
import Data.ByteString.Char8 (unpack)
import Data.Csv (Header, FromField(..), FromNamedRecord(..), (.:), decodeByName)
import Data.Vector (unsafeHead, toList)
import Data.Time (defaultTimeLocale)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (parseTimeM)
import Data.Time.Clock.POSIX (getCurrentTime)
import Network.Wreq (get, responseBody)

import Web.Data.Yahoo.Utils (right)
import Web.Data.Yahoo.Request
    ( YahooRequest(..),
      TimeRange(Before, After, Range),
      Interval(Daily, Weekly),
      Ticker(..),
      requestUrl )

type Request = YahooRequest

data Price = Price {
    date     :: Day,
    open     :: Double,
    high     :: Double,
    low      :: Double,
    close    :: Double,
    adjClose :: Double,
    volume   :: Double
} deriving (Show, Eq)

instance FromField Day where
    parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

instance FromNamedRecord Price where
    parseNamedRecord r = 
        Price 
            <$> r .: "Date" 
            <*> r .: "Open"
            <*> r .: "High"
            <*> r .: "Low"
            <*> r .: "Close"
            <*> r .: "Adj Close"
            <*> r .: "Volume"

-- | Sends a request and returns a list of prices or an error message.
fetch :: YahooRequest -> IO (Either String [Price])
fetch request = do
    response <- get $ requestUrl request
    return $ right (toList . snd) . decodeByName $ response ^. responseBody

-- | Sends a request for the specified ticker and returns its latest prices or an error code.
fetchLatest :: String -> IO (Either String Price)
fetchLatest ticker = do
    response <- get . requestUrl . request $ ticker
    return $ right (unsafeHead . snd) . decodeByName $ response ^. responseBody

-- | Creates an unparameterized request for the ticker provided. 
-- If the request gets send without specifying any additional parameters, it'll return the latest price(s).
request :: String -> YahooRequest
request t = YahooRequest {
    ticker   = Ticker t,
    interval = Nothing,
    period   = Nothing
}

-- | Specifies the request to query for daily data.
--
-- >>> fetch $ after (day 2021 01 07) . withDaily . request $ "RSX"
-- Right [Price {date = 2021-01-07, ...}, {date = 2021-01-08, ...} ...]
withDaily :: YahooRequest -> YahooRequest
withDaily (YahooRequest {ticker = t, period = p}) = YahooRequest {
    ticker   = t,
    interval = Just Daily,
    period   = p
}

-- | Specifies the request to query for weekly data. 
-- Note that Yahoo returns data for a beginning of every week specified, including the ones that don't fully fit within the specified range.
-- This might mean that the first record returned might refer to a day that comes before the time range specified in the request.
--
-- >>> fetch $ after (day 2021 01 08) . withWeekly . request $ "RSX"
-- Right [Price {date = 2021-01-04, ...},Price {date = 2021-01-11, ...}, ...]
withWeekly :: YahooRequest -> YahooRequest
withWeekly (YahooRequest {ticker = t, period = p}) = YahooRequest {
    ticker   = t,
    interval = Just Weekly,
    period   = p
}

after :: Day -> YahooRequest -> YahooRequest
after day (YahooRequest {ticker = t, interval = i}) = YahooRequest {
    ticker   = t,
    interval = i,
    period   = Just $ After day
}

before :: Day -> YahooRequest -> YahooRequest
before day (YahooRequest {ticker = t, interval = i}) = YahooRequest {
    ticker   = t,
    interval = i,
    period   = Just $ Before day
}

between :: (Day, Day) -> YahooRequest -> YahooRequest
between (from, to) (YahooRequest {ticker = t, interval = i}) = YahooRequest {
    ticker   = t,
    interval = i,
    period   = Just $ Range from to
}

day :: Integer -> Int -> Int -> Day
day = fromGregorian