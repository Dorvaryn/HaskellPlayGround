module Main 
where

import System.Environment
import FizzBuzz

iterations :: Int -> [Int]
iterations n = [1..n]

main :: IO()
main = do
    (n:_) <- getArgs
    putStrLn . unlines . fizzbuzz . iterations $  read n
