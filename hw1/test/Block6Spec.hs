module Block6Spec where

import Block6Task1And2
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Data.Char (isUpper)


okTest :: SpecWith()
okTest =
  describe "Block6.ok" $ do
    it "parse with ok shouldn't fail and consume input" $ runParser ok "asasd" `shouldBe` Just ((), "asasd")
    it "parse with ok shouldn't fail and consume input " $ runParser ok ["a","s","d"] `shouldBe` Just ((), ["a","s","d"])
    it "parse with ok shouldn't fail and consume input" $ runParser ok [3132132 :: Int] `shouldBe` Just ((), [3132132])

eofTest :: SpecWith()
eofTest =
  describe "Block6.eof" $ do
    it "Returns Nothing because of non-empty string" $ runParser eof "asasd" `shouldBe` Nothing
    it "Returns Just because of Nothing" $ runParser eof ([] :: [Int]) `shouldBe` Just ((), [])
    it "parse with ok shouldn't fail and consume input" $ runParser eof "" `shouldBe` Just ((), [])

satisfyTest :: SpecWith()
satisfyTest =
  describe "Block6.satisfy" $ do
    it "Doesn't satisfy `isUpper` predicate" $
      runParser (satisfy (isUpper :: Char -> Bool)) "sas" `shouldBe` Nothing
    it "Do satisfy `isUpper` predicate" $
      runParser (satisfy (isUpper :: Char -> Bool)) "Asas" `shouldBe` Just ('A',"sas")
    it "Random test with another predicate" $
      runParser (satisfy (== 'A')) "Asas" `shouldBe` Just ('A',"sas")

elementTest :: SpecWith()
elementTest =
  describe "Block6.element" $ do
  it "Should return Nothing because 'a' isn't first element of parsed string" $
        runParser (element 'a') "Asas" `shouldBe` Nothing
  it "Should return Just because first element is 'A'" $
        runParser (element 'A') "Asas" `shouldBe` Just ('A',"sas")

streamTest :: SpecWith()
streamTest =
  describe "Block6.stream" $ do
    it "Should return Just and consumes input because first 2 elements are valid" $
      runParser (stream "As") "Asas" `shouldBe` Just ("As", "as")
    it "Should return Nothing because the beginning of string isn't valid" $
      runParser (stream "as") "Asas" `shouldBe` Nothing
    it "Should return Nothing because the beginning of string isn't valid" $
      runParser (stream "") "Asas" `shouldBe` Just ("", "Asas")



parseBracketsTest :: SpecWith()
parseBracketsTest =
  describe "Block6.parseBrackets" $ do
    it "Spaces in the end, should return Nothing" $
      runParser parseBrackets "()()()()()()  " `shouldBe` Nothing
    it "Empty string test" $
      runParser parseBrackets "" `shouldBe` Just ((), "")
    it "Correct brackets sequence" $
      runParser parseBrackets "((((((()))))))" `shouldBe` Just ((), "")
    it "Incorrect brackets sequence" $
          runParser parseBrackets "((()(()))))))" `shouldBe` Nothing
    it "Spaces between" $
          runParser parseBrackets "(((((   (()))))))" `shouldBe` Nothing
