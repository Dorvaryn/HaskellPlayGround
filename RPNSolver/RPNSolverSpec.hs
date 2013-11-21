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
