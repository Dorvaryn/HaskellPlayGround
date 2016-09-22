module BowlingSpec where

import Test.Hspec
import Bowling

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "score" $ do
        it "is 0 for all gutters" $ do
            (score $ replicate 20 0) `shouldBe` 0
        it "is summing score of every ball played" $ do
            (score $ replicate 20 1) `shouldBe` 20
            (score $ replicate 20 3) `shouldBe` 60
        it "is adding next ball bonus if frame is spare" $ do
            (score $ 5:5:3: (replicate 17 0)) `shouldBe` 16
            (score $ 5:5: (replicate 18 1)) `shouldBe` 29
        it "is adding next two balls bonus if frame is strike" $ do
            (score $ 10:3:4: (replicate 16 0)) `shouldBe` 24
            (score $ 10: replicate 18 1) `shouldBe` 30
        it "is 300 for perfect game" $ do
            (score $ replicate 12 10) `shouldBe` 300
        it "is 150 for all spares" $ do
            (score $ replicate 21 5) `shouldBe` 150
        it "is working for strike followed by spare" $ do
            (score $ 10:5:5: (replicate 16 1)) `shouldBe` 47
        it "is working for spare followed by strike" $ do
            (score $ 5:5:10: (replicate 16 1)) `shouldBe` 48
