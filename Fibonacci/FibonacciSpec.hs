module FibonacciSpec where

import Test.Hspec
import Fibonacci

main :: IO()
main = hspec spec


spec :: Spec
spec = do
    describe "slowestFibonacci" $ do
        it "starts with 0 and 1" $ do
            slowestFibonacci 0 `shouldBe` 0
            slowestFibonacci 1 `shouldBe` 1
        it "sum the two previous terms to compute the next one" $ do
            slowestFibonacci 3 `shouldBe` 2
            slowestFibonacci 5 `shouldBe` slowestFibonacci 4 + slowestFibonacci 3
        it "of 20 is 6765" $ do
            slowestFibonacci 20 `shouldBe` 6765
    describe "slowFibonacci" $ do
        it "starts with 0 and 1" $ do
            slowFibonacci!!0 `shouldBe` 0
            slowFibonacci!!1 `shouldBe` 1
        it "sum the two previous terms to compute the next one" $ do
            slowFibonacci!!3 `shouldBe` 2
            slowFibonacci!!5 `shouldBe` slowFibonacci!!4 + slowFibonacci!!3
        it "of 20 is 6765" $ do
            slowFibonacci!!20 `shouldBe` 6765
    describe "canonicalFibonacci" $ do
        it "starts with 0 and 1" $ do
            canonicalFibonacci!!0 `shouldBe` 0
            canonicalFibonacci!!1 `shouldBe` 1
        it "sum the two previous terms to compute the next one" $ do
            canonicalFibonacci!!3 `shouldBe` 2
            canonicalFibonacci!!5 `shouldBe` canonicalFibonacci!!4 + canonicalFibonacci!!3
        it "of 20 is 6765" $ do
            canonicalFibonacci!!20 `shouldBe` 6765
    describe "fibonacci" $ do
        it "starts with 0 and 1" $ do
            fibonacci!!0 `shouldBe` 0
            fibonacci!!1 `shouldBe` 1
        it "sum the two previous terms to compute the next one" $ do
            fibonacci!!3 `shouldBe` 2
            fibonacci!!5 `shouldBe` fibonacci!!4 + fibonacci!!3
        it "of 20 is 6765" $ do
            fibonacci!!20 `shouldBe` 6765
    describe "fastFibonacci" $ do
        it "starts with 0 and 1" $ do
            fastFibonacci 0 `shouldBe` 0
            fastFibonacci 1 `shouldBe` 1
        it "sum the two previous terms to compute the next one" $ do
            fastFibonacci 3 `shouldBe` 2
            fastFibonacci 5 `shouldBe` fastFibonacci 4 + fastFibonacci 3
        it "of 20 is 6765" $ do
            fastFibonacci 20 `shouldBe` 6765
    describe "fastestFibonacci" $ do
        it "starts with 0 and 1" $ do
            fastestFibonacci 0 `shouldBe` 0
            fastestFibonacci 1 `shouldBe` 1
        it "sum the two previous terms to compute the next one" $ do
            fastestFibonacci 3 `shouldBe` 2
            fastestFibonacci 5 `shouldBe` fastestFibonacci 4 + fastestFibonacci 3
        it "of 20 is 6765" $ do
            fastestFibonacci 20 `shouldBe` 6765
