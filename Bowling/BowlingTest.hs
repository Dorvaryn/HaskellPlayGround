module ScorerTest where

import Test.HUnit
import Bowling

testAll :: Int -> Int -> Bool
testAll each expected = (score . replicate 20 $ each) == expected

tests = [
         testAll 0 0, 
         testAll 1 20,
         (score $ 5:5:3: (replicate 17 0)) == 16,
         (score $ 10:3:4: (replicate 16 0)) == 24,
         (score . replicate 12 $ 10) == 300,
         (score $ 10:5:5: (replicate 17 0)) == 30,
         (score $ replicate 20 3) == 60
        ]

main = runTestTT $ TestList [allGutters, allOnes, oneStrikeRestOnes]

allGutters = TestCase $ assertEqual
    "All gutterballs" 0 (score $ replicate 20 0)

allOnes = TestCase $ assertEqual
    "All balls roll one" 20 (score $ replicate 20 1)

oneStrike = TestCase $ assertEqual
    "First frame is strike, UncleBobTest" 24 (score $ 10:3:4: (replicate 16 0))

oneStrikeRestOnes = TestCase $ assertEqual
    "First frame is strike, rest ones" 30 (score $ 10: replicate 18 1)

