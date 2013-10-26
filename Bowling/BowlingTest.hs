module Bowling where

import Test.HUnit
import Bowling

main = runTestTT $ TestList [
                             allGutters, 
                             allOnes,
                             allThrees,
                             oneStrike, 
                             oneStrikeRestOnes, 
                             allFramesAreStrikes,
                             oneSpare,
                             firstFrameSpareRestOnes,
                             allFramesAreSpares,
                             firstFrameStrikeSecondSpare
                            ]

allGutters = TestCase $ assertEqual
    "All gutterballs" 0 (score $ replicate 20 0)

allOnes = TestCase $ assertEqual
    "All balls roll one" 20 (score $ replicate 20 1)

allThrees = TestCase $ assertEqual
    "All balls roll three" 60 (score $ replicate 20 3)

oneStrike = TestCase $ assertEqual
    "First frame is strike, UncleBobTest" 24 (score $ 10:3:4: (replicate 16 0))

oneStrikeRestOnes = TestCase $ assertEqual
    "First frame is strike, rest ones" 30 (score $ 10: replicate 18 1)

allFramesAreStrikes = TestCase $ assertEqual
    "All frames are strikes" 300 (score $ replicate 12 10)

oneSpare = TestCase $ assertEqual
    "First frame is spare, UncleBobTest" 16 (score $ 5:5:3: (replicate 16 0))

firstFrameSpareRestOnes = TestCase $ assertEqual
    "First frame is spare, rest ones" 29 (score $ 5:5: (replicate 18 1))

allFramesAreSpares = TestCase $ assertEqual
    "All frames are spares" 150 (score $ replicate 21 5)

firstFrameStrikeSecondSpare = TestCase $ assertEqual
    "First frame is strike, second is spare" 30 (score $ 10:5:5: (replicate 17 0))
