module Main 
where

import System.Environment
import RPNSolver

main :: IO()
main = do
    args <- getArgs
    if length args == 1 then
        process $ head args
    else
        print "You need to provide one expression"

process :: String -> IO()
process s = do
    let expression = readExpression $ words s
    if validate expression then
        print $ solveRPN expression
    else
        print "Invalid expression"
