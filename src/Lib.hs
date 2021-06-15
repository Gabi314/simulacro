module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doble :: Int->Int
doble = (*2)

triple :: Int->Int
triple = (*3)
