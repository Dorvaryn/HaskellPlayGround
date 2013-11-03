module Main
where

import System.Environment
import System.Cmd
import Control.Monad
import Control.Monad.Loops
import Minesweeper

readPosition :: String -> Position
readPosition ('(':first:',':second:')':_) = (read $ first:"", read $ second:"")

readPlay :: String -> Status
readPlay "play" = Played
readPlay "clear" = None
readPlay _ = Marqued

readMove :: String -> Move
readMove input = (readPosition . head $ words input, readPlay . head . tail $ words input)

readCell :: Integer -> Integer -> Char -> Cell
readCell x y status
                    | status == '*' = ((x, y), Mine, None)
                    | otherwise = ((x, y), Empty, None)

readLine :: Integer -> Integer -> String -> [Cell]
readLine x y [] = []
readLine x y (c:cars) = (readCell x y c):(readLine x (y+1) cars)

readWorld :: Integer -> [String] -> World
readWorld x [] = []
readWorld x (l:lines) = (readLine x 0 l)++(readWorld (x+1) lines)

showLine :: [Cell] -> World -> String
showLine [] _ = ""
showLine ((_, _, None):cells) world = '.':showLine cells world
showLine ((_, Mine, Played):cells) world = 'x':showLine cells world
showLine ((pos, Empty, Played):cells) world = (head . show $ hint pos world):showLine cells world
showLine ((_, _, Marqued):cells) world = 'm':showLine cells world

showWorld :: Int -> [Cell] -> World -> [String]
showWorld _ [] _ = []
showWorld l cells world = (showLine (take l cells) world):(showWorld l (drop l cells) world)

getMove world = do
    putStrLn "Enter valid Move"
    moveLn <- getLine
    let move = readMove moveLn
    if moveValid move world then
        return move
    else
        getMove world

play width world = do
    putStrLn . unlines $ showWorld width world world
    move <- getMove world
    let nextWorld = playMove move world
    if continueGame nextWorld then
        play width nextWorld
    else
        if victory nextWorld then
            putStrLn "Yeah!"
        else
            putStrLn "Bouh!"

main :: IO()
main = do
    (fileName:_) <- getArgs
    file <- readFile fileName
    let width = length . head $ lines file
    let world = readWorld 0 $ lines file
    play width world
