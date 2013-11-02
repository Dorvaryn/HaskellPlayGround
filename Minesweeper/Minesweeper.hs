module Minesweeper
where

import Data.List
import Control.Monad

data Status = None | Played | Marqued deriving (Show, Eq)
data Content = Empty | Mine deriving (Show, Eq)

type Position = (Integer, Integer)
type Cell = (Position, Content, Status)
type Move = (Position, Status)
type World = [Cell]

hint :: Position -> World -> Int
hint pos world = numberMine (neighbours pos) world

neighbours :: Position -> [Position]
neighbours (first,second) = delete (first, second) [(first+x,second+y) | x <- [-1..1], y <- [-1..1]]

numberMine :: [Position] -> World -> Int
numberMine [] game = 0
numberMine (cell:cells) game
                             | elem (cell, Mine, None) game = 1 + numberMine cells game
                             | elem (cell, Mine, Played) game = 1 + numberMine cells game
                             | elem (cell, Mine, Marqued) game = 1 + numberMine cells game
                             | otherwise = numberMine cells game

moveValid :: Move -> World -> Bool
moveValid (pos, stat) world
                            | elem (pos, Empty, None) world = True
                            | elem (pos, Mine, None) world = True
                            | otherwise = False

playMove :: Move -> World -> World
playMove _ [] = []
playMove (pos, stat) ((oldPos, content, oldStat):rest)
                                                       | pos == oldPos = (pos, content, stat):rest
                                                       | otherwise = (oldPos, content, oldStat):(playMove (pos, stat) rest)

victory :: World -> Bool
victory [] = True
victory ((position, Empty, None):rest) = False
victory ((position, Empty, Marqued):rest) = False
victory (cell:rest) = victory rest

defeat :: World -> Bool
defeat [] = False
defeat ((position, Mine, Played):rest) = True
defeat (cell:rest) = defeat rest

continueGame :: World -> Bool
continueGame world = not (victory world || defeat world)
