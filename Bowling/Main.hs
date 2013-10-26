import Bowling

testAll :: Int -> Int -> Bool
testAll each expected = (score . replicate 20 $ each) == expected

tests = [
         testAll 0 0, 
         testAll 1 20,
         (score . replicate 12 $ 10) == 300,
         (score $ 10:5:5: (replicate 17 0)) == 30,
         (score $ replicate 20 3) == 60
        ]

-- Main code
main :: IO()
main = print tests
