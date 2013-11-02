module Main
where

import System.Environment
import System.Cmd
import GameOfLife

readCell :: Integer -> Integer -> Char -> Cell
readCell x y status
                    | status == '*' = ((x, y), Alive)
                    | otherwise = ((x, y), Dead)

readLine :: Integer -> Integer -> String -> [Cell]
readLine _ _ [] = []
readLine x y (c:cars) = (readCell x y c):(readLine x (y+1) cars)

readWorld :: Integer -> [String] -> World
readWorld _ [] = []
readWorld x (l:ls) = (readLine x 0 l)++(readWorld (x+1) ls)

showLine :: [Cell] -> String
showLine [] = ""
showLine (c:cells)
                 | snd c == Alive = '*':showLine cells
                 | otherwise = '.':showLine cells

showWorld :: Int -> World -> [String]
showWorld _ [] = []
showWorld l world = (showLine $ take l world):(showWorld l (drop l world))

playNTimes :: Int -> World -> World
playNTimes 0 world = world
playNTimes n world = playNTimes (n-1) (step world world)

play :: Int -> Double -> World -> IO()
play width time world = do
    _ <- system $ "sleep " ++ (show time)
    _ <- system "clear"
    putStrLn . unlines $ showWorld width world

display :: Int -> World -> IO()
display width world = do
    putStrLn . unlines $ showWorld width world
    putStrLn "\n\n"


main :: IO()
main = do
    (fileName:st:rest) <- getArgs
    file <- readFile fileName
    let width = length . head $ lines file
    let world = readWorld 0 $ lines file
    let steps = read st
    let worlds = [playNTimes n world | n <- [0..steps]]
    if rest /= [] 
        then
            mapM_ (play width (read $ head rest)) worlds
        else
            mapM_ (display width) worlds
