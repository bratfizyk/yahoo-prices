{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where
    
import Control.Lens ((^.))
import Data.ByteString.Char8 (unpack)
import Data.Csv (FromField(..), FromNamedRecord(..), (.:), decodeByName)
import Data.Time (defaultTimeLocale)
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM)
import Network.Wreq (get, responseBody)

import qualified Data.Vector as V

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

url :: String
url = "http://query1.finance.yahoo.com/v7/finance/download/"

ticker :: String
ticker = "C38U.SI"

processPrice :: Price -> IO ()
processPrice = putStrLn . show

someFunc :: IO ()
someFunc = do
    r <- get (url ++ ticker)
    let body = r ^. responseBody
    case decodeByName body of
        Left err -> putStrLn err
        Right (_, v) ->
            V.forM_ v $ processPrice
    return ()
