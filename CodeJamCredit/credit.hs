module Main where

import System.Environment
import Data.List

groupCases :: [String] -> Maybe [(Int, [Int])] -> Maybe [(Int, [Int])]
groupCases [] results = results
groupCases (total:_:items:rest) (Just results) = groupCases rest (Just ((read total, map read $ words items):results))
groupCases _ _ = Nothing

getAllResults :: [(Int, [Int])] -> [(Int, Int)] -> [(Int,Int)]
getAllResults [] results = results
getAllResults ((total, items):rest) results = getAllResults rest ((processItems items total):results)
    where processItems items total = getValidPair (pairs items) total

getValidPair :: [(Int, Int)] -> Int -> (Int, Int)
getValidPair [] _ = error "you suck ben!"
getValidPair (x:xs) total
                       | isValid x total = x
                       | otherwise = getValidPair xs total

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (x, y)
                | x <= y    =  (x, y)
                | otherwise =  (y, x)

uniques :: Ord a => [(a, a)] -> [(a, a)]
uniques =  map head . group . sort . map sortPair

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs xs = uniques . map pickTwo $ permutations xs

pickTwo :: [Int] -> (Int, Int)
pickTwo (first:second:_) = (first, second)
pickTwo _ = (-1, -1)

isValid :: (Int, Int) -> Int -> Bool
isValid (first, second) total
                              | (first + second) == total = True
                              | otherwise = False

main = do
    (fileName:_) <- getArgs
    file <- readFile fileName
    let cases = groupCases (tail $ lines file) (Just [])
    let castJust = maybe (error "Should only be called on Just a type") id
    if cases == Nothing
    then
        putStrLn "Invalid data input"
    else
        putStrLn $ unlines . map show $ getAllResults (castJust cases) []
