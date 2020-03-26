module Block1Task2Spec where

import           Block1Task2
import           Test.Hspec  (SpecWith, describe, it, shouldBe)

fromIntTest :: SpecWith ()
fromIntTest =
  describe "Block1Task1.fromInt" $ do
    it "Returns converted fromInt natural number" $ fromInt 5 `shouldBe` S (S (S (S (S Z))))
    it "Returns 0 to check Z constructor" $ fromInt 0 `shouldBe` Z

addTest :: SpecWith ()
addTest =
  describe "Block1Task1.add" $ do
    it "Returns the sum of two naturals number" $ add (fromInt 5) (fromInt 12) `shouldBe` fromInt 17
    it "Just another test for bigger sum" $ add (fromInt 100) (fromInt 200) `shouldBe` fromInt 300

multiplyTest :: SpecWith ()
multiplyTest =
  describe "Block1Task1.multiply" $ do
    it "Returns the multiplication of 2 natural numbers" $ multiply (fromInt 5) (fromInt 12) `shouldBe` fromInt 60
    it "Checks the multiplication by zero" $ multiply (fromInt 0) (fromInt 100) `shouldBe` fromInt 0

subtractionTest :: SpecWith ()
subtractionTest =
  describe "Block1Task1.subtraction" $
  it "Returns the subtaction on two naturals number (first one is necessarily bigger than the other)" $
  subtraction (fromInt 12) (fromInt 5) `shouldBe` fromInt 7

checkEqualityTest :: SpecWith ()
checkEqualityTest =
  describe "Block1Task1.checkEquality" $ do
    it "Checks if two natural numers are equal" $ checkEquality (fromInt 12) (fromInt 12) `shouldBe` True
    it "Checks if two natural numbers are non-equal" $ checkEquality (fromInt 12) (fromInt 0) `shouldBe` False 
