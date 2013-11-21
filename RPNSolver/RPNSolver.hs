module RPNSolver
where

data Token a = Add | Sub | Mul | Div | Val a deriving Show

type Expression a = [Token a]

type Values a = [a]

solveRPN :: String -> Double
solveRPN xs = head $ foldl solve [] $ readExpression $ words xs

readExpression :: [String] -> Expression Double
readExpression = map readToken

readToken :: String -> Token Double
readToken "+" = Add
readToken "-" = Sub
readToken "*" = Mul
readToken "/" = Div
readToken  v = Val $ read v

solve :: Values Double -> Token Double -> Values Double
solve xs (Val v) = v:xs
solve (x:y:rest) op = (y `operation` x):rest
    where operation = case op of Add -> (+)
                                 Sub -> (-)
                                 Mul -> (*)
                                 Div -> (/)
