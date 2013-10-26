module Main where

import System.Environment
import Credit

prefixes = ["Case #" ++ show n ++ ": " | n <- [1..]]

prefixLines :: [String] -> [String]
prefixLines = zipWith (++) prefixes

showCustom :: Show a => (Maybe a, Maybe a) -> String
showCustom (f,s) = showMaybe f ++ " " ++ showMaybe s

showMaybe :: Show a => (Maybe a) -> String
showMaybe (Just a) = show a
showMaybe Nothing = "Nothing"

groupCases :: [String] -> Maybe [(Int, [Int])] -> Maybe [(Int, [Int])]
groupCases [] results = results
groupCases (total:_:items:rest) (Just results) = groupCases rest (Just ((read total, map read $  words items):results))
groupCases _ _ = Nothing

main = do
    (fileName:_) <- getArgs
    file <- readFile fileName
    let cases = groupCases (tail $ lines file) (Just [])
    let castJust = maybe (error "Should only be called on Just a type") id
    if cases == Nothing
    then
        putStrLn "Invalid data input"
    else
        putStrLn $ unlines . prefixLines . map showCustom $ getAllResults (castJust cases) []
