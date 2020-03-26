module Block5Task1Spec where

import Block5Task1
import Test.Hspec (SpecWith, describe, it, shouldBe)

evalTest :: SpecWith()
evalTest = 
  describe "Block4Task1.eval" $ do
    it "calculates eval of expression 77 - 8" $ eval (Subtract (Constant 77) (Constant 8)) `shouldBe` Right 69
    it "calculates eval of expression 77 + 8" $ eval (Sum (Constant 77) (Constant 8)) `shouldBe` Right 85
    it "calculates eval of expression 77 * 8" $ eval (Multiply (Constant 77) (Constant 8)) `shouldBe` Right 616
    it "calculates eval of expression 72 / 8" $ eval (Div (Constant 72) (Constant 8)) `shouldBe` Right 9
    it "calculates eval of expression 2 ^ 3" $ eval (Pow (Constant 2) (Constant 3)) `shouldBe` Right 8
    it "shoud fail due to negative pow" $ eval (Pow (Constant 2) (Constant (-3))) `shouldBe` Left PowToNegative
    it "should fail due to division by zero" $ eval (Div (Constant 2) (Constant 0)) `shouldBe` Left DividedByZero