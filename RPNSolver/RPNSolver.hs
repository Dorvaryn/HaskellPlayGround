module RPNSolver
where

data Token a = Add | Sub | Mul | Div | Val a deriving Show

type Expression a = [Token a]

fromToken :: Token Double -> Maybe Double
fromToken (Val v) = Just v
fromToken _ = Nothing

solveRPN :: String -> Maybe Double
solveRPN xs = fromToken . head $ foldl solve [] $ readExpression $ words xs

readExpression :: [String] -> Expression Double
readExpression = map readToken

readToken :: String -> Token Double
readToken "+" = Add
readToken "-" = Sub
readToken "*" = Mul
readToken "/" = Div
readToken  v = Val $ read v

solve :: Expression Double -> Token Double -> Expression Double
solve xs (Val v) = Val v:xs
solve (Val x:Val y:rest) op = (Val (y `operation` x)):rest
    where operation = case op of Add -> (+)
                                 Sub -> (-)
                                 Mul -> (*)
                                 Div -> (/)
