{-# LANGUAGE OverloadedStrings #-}

-- | A set of data types corresponding to records returned from Yahoo API.
-- Note that this is a subject to modifications whenever the remote API changes on Yahoo side.
module Web.Data.Yahoo.Response (PriceResponse(..), tryParse, tryParseAsPrice) where

import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString)
import Data.Csv (FromField(..), FromNamedRecord(..), (.:), decodeByName)
import Data.Time (defaultTimeLocale)
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM)
import Data.Vector (toList)

import Web.Data.Yahoo.Utils (right)

instance FromField Day where
    parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

-- | A type representing market price data returned by Yahoo.
data PriceResponse = PriceResponse {
    date     :: Day,
    open     :: Double,
    high     :: Double,
    low      :: Double,
    close    :: Double,
    adjClose :: Double,
    volume   :: Double
} deriving (Show, Eq)

instance FromNamedRecord PriceResponse where
    parseNamedRecord r = 
        PriceResponse 
            <$> r .: "Date" 
            <*> r .: "Open"
            <*> r .: "High"
            <*> r .: "Low"
            <*> r .: "Close"
            <*> r .: "Adj Close"
            <*> r .: "Volume"

-- | An auxiliary function that attempts to parse a string provided and interpret it as a CSV set of values of type a.
tryParse :: FromNamedRecord a => ByteString -> Either String [a]
tryParse = right (toList . snd) . decodeByName

-- | A specialized version of tryParse dedicated to parsing PriceResponse records
tryParseAsPrice :: ByteString -> Either String [PriceResponse]
tryParseAsPrice = tryParse