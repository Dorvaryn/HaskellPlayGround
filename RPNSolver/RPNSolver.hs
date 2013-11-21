module RPNSolver
where

data Token a = Add | Sub | Mul | Div | Val a deriving Show

type Expression a = [Token a]

solveRPN :: String -> Double
solveRPN xs = solve (readExpression $ words xs) []

readExpression :: [String] -> Expression Integer
readExpression = map readToken

readToken :: String -> Token Integer
readToken "+" = Add
readToken "-" = Sub
readToken "*" = Mul
readToken "/" = Div
readToken  v = Val $ read v

solve :: Expression Integer -> Expression Double -> Double
solve [] (Val v:[]) = v
solve (Val v:rest) stack = solve rest (Val (fromInteger v):stack)
solve (op:rest) (Val x:Val y:stack) = solve rest (Val (y `operation` x):stack)
    where operation = case op of Add -> (+)
                                 Sub -> (-)
                                 Mul -> (*)
                                 Div -> (/)
