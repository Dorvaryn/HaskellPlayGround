module Minesweeper
where

import Data.List
import Control.Monad
import System.Random
import System.IO.Unsafe

data Status = None | Played | Marqued deriving (Show, Eq)
data Content = Empty | Mine deriving (Show, Eq, Bounded)

type Position = (Int, Int)
type Cell = (Position, Content, Status)
type Move = (Position, Status)
type World = [Cell]

instance Enum Content where
    toEnum 0 = Empty
    toEnum 1 = Empty
    toEnum 2 = Empty
    toEnum 3 = Mine

    fromEnum Empty = 0
    fromEnum Mine = 3

instance Random Content where
    random g = case randomR (fromEnum (minBound :: Content), fromEnum (maxBound :: Content)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

generateGrid :: Int -> Int -> World
generateGrid m n = [((i,j), head . randoms $ unsafePerformIO newStdGen, None) | i <- [1..m], j <- [1..n]]

hint :: Position -> World -> Int
hint pos world = numberMine (neighbours pos) world

neighbours :: Position -> [Position]
neighbours (first,second) = delete (first, second) [(first+x,second+y) | x <- [-1..1], y <- [-1..1]]

positionToPlay :: Position -> Move
positionToPlay pos = (pos, Played)

discoverableMoves :: Position -> World -> [Move]
discoverableMoves pos world = map positionToPlay $ discoverableNeighbours pos world

discoverableNeighbours :: Position -> World -> [Position]
discoverableNeighbours pos world = filter (discoverable world) (neighbours pos)

discoverable :: World -> Position -> Bool
discoverable world pos = (elem (pos, Empty, Marqued) world || elem (pos, Empty, None) world) && (hint pos world) == 0


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
                            | stat == None && elem (pos, Mine, Marqued) world = True
                            | stat == None && elem (pos, Empty, Marqued) world = True
                            | otherwise = False

changeCellStatus :: Move -> World -> World
changeCellStatus _ [] = []
changeCellStatus (pos, stat) ((oldPos, content, oldStat):rest)
                                                       | pos == oldPos = (pos, content, stat):rest
                                                       | otherwise = (oldPos, content, oldStat):(changeCellStatus (pos, stat) rest)

playMove :: Move -> World -> World
playMove move world = uncoverNeighbours move $ changeCellStatus move world

uncoverNeighbours :: Move -> World -> World
uncoverNeighbours (pos, _) world = foldr playMove world (discoverableMoves pos world)

uncoverMines :: World -> World
uncoverMines [] = []
uncoverMines ((pos, Mine, stat):cells) = ((pos, Mine, Played):uncoverMines cells)
uncoverMines (any:cells) = any:(uncoverMines cells)

victory :: World -> Bool
victory [] = True
victory ((position, Mine, None):rest) = False
victory ((position, Mine, Played):rest) = False
victory (cell:rest) = victory rest

defeat :: World -> Bool
defeat [] = False
defeat ((position, Mine, Played):rest) = True
defeat (cell:rest) = defeat rest

continueGame :: World -> Bool
continueGame world = not (victory world || defeat world)
