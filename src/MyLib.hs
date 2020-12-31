{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Control.Lens ((^.))
import Data.ByteString.Lazy.Char8 (unpack)
import Network.Wreq (get, responseBody)

url :: String
url = "http://query1.finance.yahoo.com/v7/finance/download/"

ticker :: String
ticker = "OGZPY"

someFunc :: IO ()
someFunc = do
    r <- get (url ++ ticker)
    let body = r ^. responseBody
    putStrLn (unpack body)
    return ()
