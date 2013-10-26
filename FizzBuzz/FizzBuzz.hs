module FizzBuzz (fizzbuzz)
where

showFizzBuzz :: (Integral a, Show a) => a -> String
showFizzBuzz i 
               | i `mod` 3 == 0 = showFizz i
               | i `mod` 5 == 0 = "Buzz!"
               | otherwise = show i

showFizz :: (Integral a, Show a) => a -> String
showFizz i 
           | i `mod` 5 == 0 = "FizzBuzz!"
           | otherwise = "Fizz!"

fizzbuzz :: (Integral a, Show a) => [a] -> [String]
fizzbuzz = map showFizzBuzz

