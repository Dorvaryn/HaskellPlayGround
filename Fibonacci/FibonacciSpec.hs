module FibonacciSpec where

import Test.Hspec
import Fibonacci

main :: IO()
main = hspec $ do
    describe "slowestFibonacci" $ do
        it "starts with 0 and 1" $ do
            slowestFibonacci!!0 `shouldBe` 0
            slowestFibonacci!!1 `shouldBe` 1
        it "sum the two previous terms to compute the next one" $ do
            slowestFibonacci!!3 `shouldBe` 2
            slowestFibonacci!!5 `shouldBe` slowestFibonacci!!4 + slowestFibonacci!!3
        it "of 20 is 6765" $ do
            slowestFibonacci!!20 `shouldBe` 6765
    describe "slowFibonacci" $ do
        it "starts with 0 and 1" $ do
            slowFibonacci!!0 `shouldBe` 0
            slowFibonacci!!1 `shouldBe` 1
        it "sum the two previous terms to compute the next one" $ do
            slowFibonacci!!3 `shouldBe` 2
            slowFibonacci!!5 `shouldBe` slowFibonacci!!4 + slowFibonacci!!3
        it "of 20 is 6765" $ do
            slowFibonacci!!20 `shouldBe` 6765
    describe "fibonacci" $ do
        it "starts with 0 and 1" $ do
            fibonacci!!0 `shouldBe` 0
            fibonacci!!1 `shouldBe` 1
        it "sum the two previous terms to compute the next one" $ do
            fibonacci!!3 `shouldBe` 2
            fibonacci!!5 `shouldBe` fibonacci!!4 + fibonacci!!3
        it "of 20 is 6765" $ do
            fibonacci!!20 `shouldBe` 6765