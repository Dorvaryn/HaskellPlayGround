module Main
where

import System.Environment
import GameOfLife

readCell :: Integer -> Integer -> Char -> Cell
readCell x y status
                    | status == '*' = ((x, y), Alive)
                    | otherwise = ((x, y), Dead)

readLine :: Integer -> Integer -> String -> [Cell]
readLine x y [] = []
readLine x y (c:cars) = (readCell x y c):(readLine x (y+1) cars)

readWorld :: Integer -> [String] -> World
readWorld x [] = []
readWorld x (l:lines) = (readLine x 0 l)++(readWorld (x+1) lines)

showLine :: [Cell] -> String
showLine [] = ""
showLine (c:cells)
                 | snd c == Alive = "*"
                 | otherwise = "."

showWorld :: Int -> World -> [String]
showWorld _ [] = []
showWorld l world = (showLine $ take l world):(showWorld l (drop l world))


main :: IO()
main = do
    (fileName:_) <- getArgs
    file <- readFile fileName
    let width = length . head $ lines file
    let world = readWorld 0 $ lines file
    print . unlines $ showWorld width $ step world world

