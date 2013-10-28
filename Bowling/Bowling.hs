module Bowling ( score )
where

score :: (Eq a, Num a) => [a] -> a
score [] = 0
score (x:[]) = x 
score (x:y:[]) = x + y
score (x:y:z:[]) = x + y + z
score (x:y:z:xs)   | x == 10 = x + y + z + score (y:z:xs)
                   | (x + y) == 10 = x + y + z + score (z:xs)
                   | otherwise = x + y + score (z:xs)
