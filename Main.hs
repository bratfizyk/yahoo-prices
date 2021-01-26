module Main where

import Web.Data.Yahoo.API (Price, fetch, requestForTicker, withDaily, between, day) 

testFunc :: IO ()
testFunc = do
    result <- fetch $ between (day 2021 01 08, day 2021 01 15) . withDaily . requestForTicker $ "RSX"
    case result of
        Left err -> putStrLn err
        Right v  -> mapM_ processPrice v
    return ()

    where
        processPrice :: Price -> IO ()
        processPrice = putStrLn . show

main :: IO ()
main = do
  putStrLn "Testing Library"
  testFunc
