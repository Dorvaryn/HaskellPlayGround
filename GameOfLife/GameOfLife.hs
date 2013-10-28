module WorldOfLife
where

import Data.List
import Control.Monad

data Status = Dead | Alive deriving (Show, Eq)

type Position = (Integer, Integer)
type Cell = (Position, Status)
type World = [Cell]

neighbours :: Cell -> [Position]
neighbours ((first,second),_) = [(first+x,second+y) | x <- [-1..1], y <- [-1..1]]

numberAlive :: [Position] -> World -> Int
numberAlive [] game = 0
numberAlive (cell:cells) game 
                                  | elem (cell, Alive) game = 1 + numberAlive cells game
                                  | otherwise = numberAlive cells game

step :: World -> World -> World
step [] world = []
step (cell:cells) world
                        | (numberAlive (neighbours cell) world == 2) = (fst cell, Alive):(step cells world)
                        | (numberAlive (neighbours cell) world == 3) = (fst cell, Alive):(step cells world)
                        | otherwise = (fst cell, Dead):(step cells world)
