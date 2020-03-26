module Block4Task1Spec
  (stringSumTest)
  where

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Block4Task1

stringSumTest :: SpecWith ()
stringSumTest =
  describe "Block4Task1.stringSum" $ do
    it "calculates sum properly in default string" $ stringSum "9 2 3 2 7 1 0 82 2" `shouldBe` Just 108
    it "returns Nothing, when not only numbers appear in the string (chars)" $ stringSum "1 2 a b c s w" `shouldBe` Nothing
    it "parses numbers from string with a lof of spaces between numbers" $ stringSum "1 2     \t \n 3" `shouldBe` Just 6
