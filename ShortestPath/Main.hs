module Main
where

import System.Environment
import ShortestPath

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n xs = take n xs:(groupBy n $ drop n xs)

readRoadSystem :: [[String]] -> RoadSystem
readRoadSystem = map (\[a,b,c] -> Section a b c) . (map $ map read)

main = do
    (fileName:_) <- getArgs
    file <- readFile fileName
    print $ shortestPath . readRoadSystem . (groupBy 3) $ lines file
