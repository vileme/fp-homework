module Block2Task2Spec where


import Block2Task2
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Data.List.NonEmpty

splitOnTest :: SpecWith ()
splitOnTest =
  describe "Block1Task1.splitOn" $ do
    it "Sample test" $ splitOn '/' "path/to/file" `shouldBe` "path" :| ["to","file"]
    it "Test with no given delimiter so must be non-empty list" $ splitOn ';' "2312312312312321" `shouldBe` "2312312312312321" :| []
    it "Test with delimiter in the end of string" $ splitOn ';' "asd;" `shouldBe` "asd" :| [""]
    it "Test with non char" $ splitOn '1' "1 232323 1 2" `shouldBe` "" :| [" 232323 "," 2"]


    



