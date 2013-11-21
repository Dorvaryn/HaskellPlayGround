module RPNSolver
where

data Token a = Add | Sub | Mul | Div | Val a deriving Show

type Expression a = [Token a]

type Values a = [a]

validate :: Num a => Expression a -> Bool
validate stack 
    | stackDepth == 1 = True
    | otherwise = False
    where stackDepth = foldl depth 0 stack

depth :: Integer -> Token a -> Integer
depth d (Val _) = d + 1
depth d _ = d - 1

solveRPN :: Expression Double -> Double
solveRPN xs = head $ foldl solve [] xs

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
