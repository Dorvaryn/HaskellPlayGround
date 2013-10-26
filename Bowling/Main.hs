import Bowling

test1 = (score . replicate 20 $ 0) == 0 -- 0
test2 = (score . replicate 12 $ 10) == 300--300
test3 = (score $ 10:5:5: (replicate 17 0)) == 30 --30
test4 = (score $ replicate 20 3) == 60 --60

-- Main code
main :: IO()
main = print [test1, test2, test3, test4]
