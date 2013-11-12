module Main
where

import System.Environment
import System.Cmd
import Control.Monad
import Control.Monad.Loops
import Minesweeper

readPosition :: String -> Position
readPosition ('(':first:',':second:')':_) = (read $ first:"", read $ second:"")
readPosition ('(':first:', ':second:')':_) = (read $ first:"", read $ second:"")

readPlay :: String -> Status
readPlay "play" = Played
readPlay "clear" = None
readPlay _ = Marqued

readMove :: String -> Move
readMove input = (readPosition . head $ words input, readPlay . head . tail $ words input)

readCell :: Int -> Int -> Char -> Cell
readCell x y status
                    | status == '*' = ((x, y), Mine, None)
                    | otherwise = ((x, y), Empty, None)

readLine :: Int -> Int -> String -> [Cell]
readLine x y [] = []
readLine x y (c:cars) = (readCell x y c):(readLine x (y+1) cars)

readWorld :: Int -> [String] -> World
readWorld x [] = []
readWorld x (l:lines) = (readLine x 1 l)++(readWorld (x+1) lines)

showLine :: [Cell] -> World -> String
showLine [] _ = ""
showLine ((_, _, None):cells) world = '.':' ':showLine cells world
showLine ((_, Mine, Played):cells) world = 'x':' ':showLine cells world
showLine ((pos, Empty, Played):cells) world 
                                            | numHint == '0' = '-':' ':showLine cells world
                                            | otherwise = numHint:' ':showLine cells world
                                                where numHint = (head . show $ hint pos world)
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
    move <- getMove world
    let nextWorld = playMove world move
    if continueGame nextWorld then
        do
            putStrLn . unlines $ showWorld width nextWorld nextWorld
            play width nextWorld
    else
        do
            if victory nextWorld then do
                putStrLn "Yeah!"
                putStrLn . unlines $ showWorld width nextWorld nextWorld;
            else do
                putStrLn "Bouh!"
                let endWorld = uncoverMines nextWorld
                putStrLn . unlines $ showWorld width endWorld endWorld;

main :: IO()
main = do
    args <- getArgs
    if args /= [] then do
        let fileName = head args
        file <- readFile fileName
        let width = length . head $ lines file
        let world = readWorld 1 $ lines file
        putStrLn . unlines $ showWorld width world world
        play width world
    else do
        let width = 10
        let world = generateGrid 10 10
        putStrLn . unlines $ showWorld width world world
        play width world

