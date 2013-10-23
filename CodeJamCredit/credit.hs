module Main where

import System.Environment
import Data.List

groupCases :: [String] -> Maybe [(Int, [Int])] -> Maybe [(Int, [Int])]
groupCases [] results = results
groupCases (total:_:items:rest) (Just results) = groupCases rest (Just ((read total, map read $  words items):results))
groupCases _ _ = Nothing

getAllResults :: [(Int, [Int])] -> [(Maybe Int, Maybe Int)] -> [(Maybe Int, Maybe Int)]
getAllResults [] results = results
getAllResults ((total, items):rest) results = getAllResults rest ((processItems items total):results)
    where processItems items total = getPositions (getValidPair (pairs items) total) items

getPositions :: (Int, Int) -> [Int] -> (Maybe Int, Maybe Int)
getPositions (first, second) items 
                                    | elemIndices first items == elemIndices second items = getOneIndexedPair (elemIndex first items) (Just (head . tail $ elemIndices second items))
                                    | otherwise = getOneIndexedPair (elemIndex first items) (elemIndex second items)

getOneIndexedPair :: Maybe Int -> Maybe Int -> (Maybe Int, Maybe Int)
getOneIndexedPair first second = (fmap (1+) first, fmap(1+) second)

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
