module Main 
where

import System.Environment
import Fibonacci

main :: IO()
main = do
    (n:_) <- getArgs
    print $ fibonacci!!(read n)
