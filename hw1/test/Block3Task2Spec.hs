module Block3Task2Spec where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import Block3Task2

nonEmptySemigroup :: SpecWith()
nonEmptySemigroup =
  describe "Block1Task1.nonEmptySemigroupInstance" $
  it "Sample test" $
  (2 :| [2, 3, 4, 5] :: NonEmpty Int) <> 3 :| [2, 2, 2, 2, 2, 2, 4] `shouldBe` 2 :| [2, 3, 4, 5, 3, 2, 2, 2, 2, 2, 2, 4]


thisOrThatTest :: SpecWith()
thisOrThatTest =
  describe "Block1Task1.thisOrThatTest" $ do
    it "(Both x y) (This a)" $
      (Both [1, 2, 3] [2] :: (ThisOrThat [Int] [Int])) <> This [2, 2] `shouldBe` Both [1, 2, 3, 2, 2] [2]
    it "(Both x y) (Both x y)" $
      (Both [1, 2, 3] [2] :: (ThisOrThat [Int] [Int])) <> (Both [1, 2, 3] [2] :: (ThisOrThat [Int] [Int])) `shouldBe`
      Both [1, 2, 3, 1, 2, 3] [2, 2]
    it "(This x) (This y)" $
      (This [2, 2] :: ThisOrThat [Int] [Int]) <> (This [2, 2] :: ThisOrThat [Int] [Int]) `shouldBe` This [2, 2, 2, 2]
    it "(This x) (That a)" $ (This [2, 2] :: ThisOrThat [Int] [Int]) <> That [2, 2] `shouldBe` Both [2, 2] [2, 2]
    it "(That a) (This a)" $ (That [2, 2] :: ThisOrThat [Int] [Int]) <> This [2, 2] `shouldBe` Both [2, 2] [2, 2]
    it "(That a) (That a)" $ (That [2, 2] :: ThisOrThat [Int] [Int]) <> That [2, 2] `shouldBe` That [2, 2, 2, 2]
    it "(This x) (Both x y)" $
      (This [2, 2] :: ThisOrThat [Int] [Int]) <> Both [1, 2, 3] [2] `shouldBe` Both [2, 2, 1, 2, 3] [2]
    it "(That b) (Both x y)" $
      (That [2, 2] :: ThisOrThat [Int] [Int]) <> Both [1, 2, 3] [2] `shouldBe` Both [1, 2, 3] [2, 2, 2]
