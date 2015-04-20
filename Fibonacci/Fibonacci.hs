module Fibonacci
where

import Matrix
import Data.List
import Data.Bits

slowestFibonacci :: Integer -> Integer
slowestFibonacci n
                   | n == 0    = 0
                   | n == 1    = 1
                   | otherwise = slowestFibonacci (n-1) + slowestFibonacci (n-2)

slowFibonacci :: [Integer]
slowFibonacci = 0 : 1 : [slowFibonacci!!(n-1) + slowFibonacci!!(n-2) | n <- [2..]]

canonicalFibonacci :: [Integer]
canonicalFibonacci = 0 : 1 : zipWith (+) canonicalFibonacci (tail canonicalFibonacci)

fibonacci :: [Integer]
fibonacci = scanl (+) 0 $ 1:fibonacci


-- Implementations under this line are not mine, examples of logarithmic implementations from haskell.org

fastFibonacci :: Integer -> Integer
fastFibonacci n = head (apply (Matrix [[0,1], [1,1]] ^ n) [0,1])

fastestFibonacci :: Int -> Integer
fastestFibonacci n = snd . foldl' fib (1, 0) . dropWhile not $
            [testBit n k | k <- let s = finiteBitSize n in [s-1,s-2..0]]
    where
        fib (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g
