{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where
    
import Control.Lens ((^.))
import Data.ByteString.Char8 (unpack)
import Data.Csv (Header, FromField(..), FromNamedRecord(..), (.:), decodeByName)
import Data.Int (Int64)
import Data.Time (UTCTime, defaultTimeLocale, nominalDiffTimeToSeconds)
import Data.Time.Calendar (Day, fromGregorian, diffDays)
import Data.Time.Format (parseTimeM)
import Data.Time.Clock.POSIX (getCurrentTime, utcTimeToPOSIXSeconds)
import Network.Wreq (get, responseBody)
import Text.Printf (printf)

import qualified Data.Vector as V

-- Request

data Ticker = Ticker String

class YahooParam a where
    key    :: a -> String
    symbol :: a -> String

data Interval = 
    Daily
    | Weekly
    | Monthly
    deriving (Show, Eq)

instance YahooParam Interval where
    key _ = "interval"
    symbol Daily = "1d"
    symbol Weekly = "1wk"
    symbol Monthly = "1mo"

data Events =
    HistoricalPrices
    | Dividends
    | Splits

instance YahooParam Events where
    key _ = "events"
    symbol HistoricalPrices = "history"
    symbol Dividends = "div"
    symbol Splits = "split"

data YahooRequest = YahooRequest {
    ticker   :: Ticker,
    interval :: Maybe Interval
}

requestUrl :: YahooRequest -> String
requestUrl (YahooRequest { ticker = (Ticker t), interval = i }) = 
    printf "%s/%s" baseUrl t
    where
        baseUrl :: String
        baseUrl = "http://query1.finance.yahoo.com/v7/finance/download"

-- Response

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

-- Fetch

fetch :: YahooRequest -> IO (Either String (Header, V.Vector Price))
fetch request = do
    response <- get $ requestUrl request
    let body = response ^. responseBody
    return $ decodeByName body

-- Test

request :: YahooRequest
request = YahooRequest {
    ticker   = Ticker "RSX",
    interval = Nothing
}

processPrice :: Price -> IO ()
processPrice = putStrLn . show

secondsSinceEpoch :: UTCTime -> Int64
secondsSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

dayAsEpoch :: Day -> Integer
dayAsEpoch day = 
    3600 * 24 * (diffDays day start)
    where
        start :: Day
        start = fromGregorian 1970 1 1

someFunc :: IO ()
someFunc = do
    result <- fetch request
    case result of
        Left err -> putStrLn err
        Right (sdf, v) ->
            V.forM_ v $ processPrice
    return ()
