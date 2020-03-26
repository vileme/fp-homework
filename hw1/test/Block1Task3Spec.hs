module Block1Task3Spec where

import           Block1Task3
import           Test.Hspec  (SpecWith, describe, it, shouldBe)
import Data.List.NonEmpty


fromListToTreeTest :: SpecWith ()
fromListToTreeTest =
  describe "Block1Task1.fromListToTree" $
  it "Returns the Tree from list" $
  fromListToTree [1, 2, 3, 4, 5 :: Int] `shouldBe`
  Node (Node (Node (Node (Node Leaf (1 :| []) Leaf) (2 :| []) Leaf) (3 :| []) Leaf) (4 :| []) Leaf) (5 :| []) Leaf

isEmptyTest :: SpecWith ()
isEmptyTest =
  describe "Block1Task1.isEmpty" $ do
    it "Return true if tree is empty" $ isEmpty Leaf `shouldBe` True
    it "Returns false if tree is non-empty" $ isEmpty (Node Leaf ((1 :: Int) :| []) Leaf) `shouldBe` False

countElementsTest :: SpecWith ()
countElementsTest =
  describe "Block1Task1.countElements" $ do
    it "Returns the number of elements on tree" $ countElements (fromListToTree [1, 2, 3, 3, 3, 3, 4, 5 :: Int]) `shouldBe` 8
    it "asdasdas" $ countElements (fromListToTree ([] :: [Int])) `shouldBe` 0

searchTest :: SpecWith ()
searchTest =
  describe "Block1Task1.search" $ do
    it "Returns true because the element was founded" $ search (4 :: Int) (fromListToTree [1,2,3,4::Int])  `shouldBe` True
    it "Returns false because the element wasn't founded" $ search (10 :: Int) (fromListToTree ([1,2,3,4] :: [Int]))  `shouldBe` False

insertXTest :: SpecWith ()
insertXTest =
  describe "Block1Task1.insertX" $
  it "Returns new tree with inserted element" $ insertX 5 (fromListToTree [1, 6, 3 :: Int]) `shouldBe`
  fromListToTree [1, 5, 6, 3 :: Int]
  
deleteTest :: SpecWith ()
deleteTest =
  describe "Block1Task1.delete" $ do
    it "Returns the new Tree with deleted element" $ delete 5 (fromListToTree [1, 5, 6, 3 :: Int]) `shouldBe` fromListToTree [1, 6, 3 :: Int]
    it "Returns the same tree because there is nothing to delete" $ delete (5 :: Int) Leaf `shouldBe` Leaf 

