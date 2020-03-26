module Block3Task1Spec where

import Block3Task1
import Test.Hspec (SpecWith, describe, it, shouldBe)


maybeConcatTest :: SpecWith ()
maybeConcatTest =
  describe "Block1Task1.maybeConcat" $ do
    it "Sample test" $ maybeConcat ([Just [1,2,3], Nothing, Just [4,5]] :: [Maybe [Int]]) `shouldBe` ([1,2,3,4,5] :: [Int])
    it "Empty Maybe list returns empty" $ maybeConcat ([Nothing, Nothing, Nothing] :: [Maybe [Int]]) `shouldBe` []
