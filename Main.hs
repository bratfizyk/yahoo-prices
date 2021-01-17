module Main where

import qualified Web.Data.Yahoo.API as Yahoo (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Yahoo.someFunc
