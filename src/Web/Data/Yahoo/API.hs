{-# LANGUAGE OverloadedStrings #-}

module Web.Data.Yahoo.API (someFunc) where
    
import Control.Lens ((^.))
import Data.ByteString.Char8 (unpack)
import Data.Csv (Header, FromField(..), FromNamedRecord(..), (.:), decodeByName)
import Data.Time (defaultTimeLocale)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (parseTimeM)
import Data.Time.Clock.POSIX (getCurrentTime)
import Network.Wreq (get, responseBody)

import Web.Data.Yahoo.Request
    ( YahooRequest(..),
      TimeRange(Range, After),
      Interval(Daily),
      Ticker(..),
      requestUrl )

import qualified Data.Vector as V

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
    putStrLn $ "Requesting: " ++ (requestUrl request)
    response <- get $ requestUrl request
    let body = response ^. responseBody
    return $ decodeByName body

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

-- Test
request :: YahooRequest
request =  after (fromGregorian 2021 01 12) . withDaily . requestForTicker . Ticker $ "RSX"

someFunc :: IO ()
someFunc = do
    result <- fetch request
    case result of
        Left err -> putStrLn err
        Right (sdf, v) ->
            V.forM_ v $ processPrice
    return ()

    where
        processPrice :: Price -> IO ()
        processPrice = putStrLn . show