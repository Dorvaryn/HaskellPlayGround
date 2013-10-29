module Fibonacci (slowestFibonacci, slowFibonacci, fibonacci)
where

slowestFibonacci :: [Integer]
slowestFibonacci = [recursiveNthFib n | n <- [0..]]

recursiveNthFib :: (Eq a, Num a) => a -> a
recursiveNthFib n
                   | n == 0    = 0
                   | n == 1    = 1
                   | otherwise = recursiveNthFib (n-1) + recursiveNthFib (n-2)


slowFibonacci :: [Integer]
slowFibonacci = 0 : 1 : [slowFibonacci!!(n-1) + slowFibonacci!!(n-2) | n <- [2..]]

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
