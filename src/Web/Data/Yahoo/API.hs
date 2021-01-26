{-# LANGUAGE OverloadedStrings #-}

module Web.Data.Yahoo.API (someFunc) where
    
import Control.Lens ((^.))
import Data.ByteString.Char8 (unpack)
import Data.Csv (Header, FromField(..), FromNamedRecord(..), (.:), decodeByName)
import Data.Vector (toList)
import Data.Time (defaultTimeLocale)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (parseTimeM)
import Data.Time.Clock.POSIX (getCurrentTime)
import Network.Wreq (get, responseBody)

import Web.Data.Yahoo.Request
    ( YahooRequest(..),
      TimeRange(Before, After, Range),
      Interval(Daily),
      Ticker(..),
      requestUrl )

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
fetch :: YahooRequest -> IO (Either String [Price])
fetch request = do
    putStrLn $ "Requesting: " ++ (requestUrl request)
    response <- get $ requestUrl request
    return $ right (toList . snd) . decodeByName $ response ^. responseBody

    where
        right :: (t -> b) -> Either a t -> Either a b
        right f (Right x) = Right (f x)
        right _ (Left x)  = Left x

-- API
requestForTicker :: Ticker -> YahooRequest
requestForTicker t = YahooRequest {
    ticker   = t,
    interval = Nothing,
    period   = Nothing
}

withDaily :: YahooRequest -> YahooRequest
withDaily (YahooRequest {ticker = t, period = p}) = YahooRequest {
    ticker   = t,
    interval = Just Daily,
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

-- Test
request :: YahooRequest
request =  between (fromGregorian 2021 01 08, fromGregorian 2021 01 15) . withDaily . requestForTicker . Ticker $ "RSX"

someFunc :: IO ()
someFunc = do
    result <- fetch request
    case result of
        Left err -> putStrLn err
        Right v  -> mapM_ processPrice v
    return ()

    where
        processPrice :: Price -> IO ()
        processPrice = putStrLn . show