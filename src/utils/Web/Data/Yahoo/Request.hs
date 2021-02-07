module Web.Data.Yahoo.Request where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Time.Calendar (Day, fromGregorian)
import Text.Printf (printf)

import Web.Data.Yahoo.Utils (dayAsEpoch)

data Ticker = Ticker String

class YahooParam a where
    key    :: a -> String
    symbol :: a -> String

paramToString :: YahooParam a => a -> String
paramToString p = printf "%s=%s" (key p) (symbol p)

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

data FromEndpoint = FromEndpoint Day

instance YahooParam FromEndpoint where
    key _ = "period1"
    symbol (FromEndpoint day) = show $ dayAsEpoch day

data ToEndpoint = ToEndpoint Day

instance YahooParam ToEndpoint where
    key _ = "period2"
    symbol (ToEndpoint day) = show $ dayAsEpoch day

data TimeRange =
    After Day
    | Before Day
    | Range Day Day

timeRangeToEndpoints :: TimeRange -> (FromEndpoint, ToEndpoint)
timeRangeToEndpoints (After day)     = (FromEndpoint day, ToEndpoint $ fromGregorian 2050 12 31)
timeRangeToEndpoints (Before day)    = (FromEndpoint $ fromGregorian 1970 1 1, ToEndpoint day)
timeRangeToEndpoints (Range from to) = (FromEndpoint from, ToEndpoint to)

data YahooRequest = YahooRequest {
    ticker   :: Ticker,
    interval :: Maybe Interval,
    period   :: Maybe TimeRange
}

requestUrl :: YahooRequest -> String
requestUrl (YahooRequest { ticker = (Ticker t), interval = i, period = p }) = 
    printf "%s/%s%s" baseUrl t (queryString queryParams)
    where
        baseUrl :: String
        baseUrl = "http://query1.finance.yahoo.com/v7/finance/download"

        rangeEndpoints :: Maybe (FromEndpoint, ToEndpoint)
        rangeEndpoints = fmap timeRangeToEndpoints p

        fromEndpoint :: Maybe FromEndpoint
        fromEndpoint = fmap fst rangeEndpoints

        toEndpoint :: Maybe ToEndpoint
        toEndpoint = fmap snd rangeEndpoints

        queryParams :: [String]
        queryParams = catMaybes $ [
                fmap paramToString i,
                fmap paramToString fromEndpoint,
                fmap paramToString toEndpoint
            ]

        queryString :: [String] -> String
        queryString [] = ""
        queryString ps = printf "?%s" (intercalate "&" ps)