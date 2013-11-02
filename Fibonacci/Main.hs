module Main 
where

import System.Environment
import Fibonacci

main :: IO()
main = do
    (n:_) <- getArgs
    print . fastestFibonacci $ read n
