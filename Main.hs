module Main where

import Web.Data.Yahoo.API (Price, Request, fetch, request, withDaily, between, day, fetchLatest) 

testFunc :: IO ()
testFunc = do
    -- result <- fetch $ between (day 2021 01 08, day 2021 01 15) . withDaily . request $ "RSX"
    -- result <- fetch $ request "RSX"
    result <- fetchLatest "RSX"
    case result of
        Left err -> putStrLn err
        -- Right v  -> mapM_ processPrice v
        Right v  -> processPrice v
    return ()

    where
        processPrice :: Price -> IO ()
        processPrice = putStrLn . show

main :: IO ()
main = do
  putStrLn "Testing Library"
  testFunc
