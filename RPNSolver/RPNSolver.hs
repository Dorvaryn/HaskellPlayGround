module RPNSolver
where

data Token a = Add | Sub | Mul | Div | Val a deriving Show

type Expression a = [Token a]

solveRPN :: String -> Integer
solveRPN = solve . readExpression . words

readExpression :: [String] -> Expression Integer
readExpression = map readToken

readToken :: String -> Token Integer
readToken "+" = Add
readToken "-" = Sub
readToken "*" = Mul
readToken "/" = Div
readToken  v = Val $ read v

solve :: Expression Integer -> Integer
solve x = 42
