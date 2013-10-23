module Main where

import System.Environment
import Data.List

reverseWords :: String -> String
reverseWords = unwords . reverse . words 

prefixes = ["Case #" ++ show n ++ ": " | n <- [1..]]

prefixLines :: [String] -> [String]
prefixLines = zipWith (++) prefixes

main = do
    (fileName:_) <- getArgs
    file <- readFile fileName
    putStrLn $ unlines . prefixLines . tail . map reverseWords $ lines file
