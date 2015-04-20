module FizzBuzzSpec where

import Test.Hspec
import FizzBuzz

main :: IO()
main = hspec spec


spec :: Spec
spec = do
    describe "fizzbuzz" $ do
            it "is fizz for 3" $ do
                fizzbuzz [3] `shouldBe` ["Fizz!"]
            it "is buzz for 5" $ do
                fizzbuzz [5] `shouldBe` ["Buzz!"]
            it "is fizzbuzz for 15" $ do
                fizzbuzz [15] `shouldBe` ["FizzBuzz!"]

