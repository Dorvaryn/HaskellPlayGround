module Main where


iterations = [1..100]

showFizzBuzz :: (Integral a, Show a) => a -> String
showFizzBuzz i 
               | i `mod` 3 == 0 = showFizz i
               | i `mod` 5 == 0 = "Buzz!"
               | otherwise = show i

showFizz :: (Integral a, Show a) => a -> String
showFizz i 
           | i `mod` 5 == 0 = "FizzBuzz!"
           | otherwise = "Fizz!"

main = do
    putStrLn . unlines $ map showFizzBuzz iterations
