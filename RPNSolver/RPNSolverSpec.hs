module RPNSolverSpec where

import Test.Hspec
import RPNSolver

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "RPN Solver" $ do
        it "(10 4 3 + 2 * -)" $ do
            (solveRPN . readExpression $ words "10 4 3 + 2 * -") `shouldBe` -4
        it "(2 3 +)" $ do
            (solveRPN . readExpression $ words "2 3 +") `shouldBe` 5
        it "(90 34 12 33 55 66 + * - +)" $ do
            (solveRPN . readExpression $ words "90 34 12 33 55 66 + * - +") `shouldBe` -3947
        it "(90 34 12 33 55 66 + * - + -)" $ do
            (solveRPN . readExpression $ words "90 34 12 33 55 66 + * - + -") `shouldBe` 4037
        it "(90 3 -)" $ do
            (solveRPN . readExpression $ words "90 3 -") `shouldBe` 87
        it "(2.7 ln)" $ do
            (solveRPN . readExpression $ words "2.7 ln") `shouldBe` 0.9932517730102834
        it "(10 10 10 10 sum 4 /)" $ do
            (solveRPN . readExpression $ words "10 10 10 10 sum 4 /") `shouldBe` 10
        it "(10 10 10 10 10 sum 4 /)" $ do
            (solveRPN . readExpression $ words "10 10 10 10 10 sum 4 /") `shouldBe` 12.5
        it "(10 2 ^)" $ do
            (solveRPN . readExpression $ words "10 2 ^") `shouldBe` 100
        it "(43.2425 0.5 ^)" $ do
            (solveRPN . readExpression $ words "43.2425 0.5 ^") `shouldBe` 6.575902979819578
