module Main 
where

import System.Environment
import ReverseWords

prefixes = ["Case #" ++ show n ++ ": " | n <- [1..]]

prefixLines :: [String] -> [String]
prefixLines = zipWith (++) prefixes

main :: IO()
main = do
    (fileName:_) <- getArgs
    file <- readFile fileName
    putStrLn $ unlines . prefixLines . tail . map reverseWords $ lines file