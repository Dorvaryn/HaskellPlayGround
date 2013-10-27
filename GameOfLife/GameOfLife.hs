module WorldOfLife
where

import Data.List
import Control.Monad

type Cell = (Int, Int)
type World = [Cell]

neighbours :: Cell -> [Cell]
neighbours (first,second) = [(first+x,second+y) | x <- [-1..1], y <- [-1..1]]

numberContained :: [Cell] -> World -> Int
numberContained (cell:cells) game 
                                  | elem cell game = 1 + numberContained cells game
                                  | otherwise = numberContained cells game
