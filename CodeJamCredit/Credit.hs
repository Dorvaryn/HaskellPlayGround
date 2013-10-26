module Credit (getAllResults)
where

import Data.List

getAllResults :: [(Int, [Int])] -> [(Maybe Int, Maybe Int)] -> [(Maybe Int, Maybe Int)]
getAllResults [] results = results
getAllResults ((total, items):rest) results = getAllResults rest ((processItems items total):results)
    where processItems items total = getPositions (getValidPair (pairs items) total) items

getPositions :: Maybe (Int, Int) -> [Int] -> (Maybe Int, Maybe Int)
getPositions Nothing items = (Nothing, Nothing)
getPositions (Just (first, second)) items 
                                    | elemIndices first items == elemIndices second items = getOneIndexedPair (elemIndex first items) (Just (head . tail $ elemIndices second items))
                                    | otherwise = getOneIndexedPair (elemIndex first items) (elemIndex second items)

getOneIndexedPair :: Maybe Int -> Maybe Int -> (Maybe Int, Maybe Int)
getOneIndexedPair first second = sortPair (fmap (1+) first, fmap(1+) second)

getValidPair :: [(Int, Int)] -> Int -> Maybe (Int, Int)
getValidPair [] _ = Nothing
getValidPair (x:xs) total
                       | isValid x total = Just x
                       | otherwise = getValidPair xs total

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (x, y)
                | x <= y    =  (x, y)
                | otherwise =  (y, x)

uniques :: Ord a => [(a, a)] -> [(a, a)]
uniques =  map head . group . sort . map sortPair

shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

allRotations :: [a] -> [[a]]
allRotations l = take (length l) (iterate shift l)

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs xs = uniques . concat $ map (zip xs) (allRotations xs)

isValid :: (Int, Int) -> Int -> Bool
isValid (first, second) total
                              | (first + second) == total = True
                              | otherwise = False

