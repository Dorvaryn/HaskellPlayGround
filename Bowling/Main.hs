module Main
where

import System.Environment
import Bowling

main :: IO()
main = do
    balls <- getArgs
    print . score . map read $ balls
