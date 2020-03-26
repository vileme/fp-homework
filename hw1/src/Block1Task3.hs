{-# LANGUAGE InstanceSigs #-}
module Block1Task3
  ( Tree (..)
    , countElements
    , delete
    , fromListToTree
    , insertX
    , isEmpty
    , search
  )  where

import Data.List.NonEmpty as NonEmpty

-- | Data class which illustrates Binary tree
data Tree a = Leaf | Node (Tree a) (NonEmpty a) (Tree a) deriving Show

instance (Eq a) => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) Leaf Leaf = True
  (==) (Node left list right) (Node scLeft scList scRight) = (left == scLeft) && (list == scList) && right == scRight
  (==) _  _ = False

-- | Checks whether the tree is empty
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Counts elements in tree
countElements :: Tree a -> Int
countElements Leaf = 0
countElements (Node left list right) = NonEmpty.length list + countElements left + countElements right

-- | Searches the element in tree 
search :: (Ord a) => a -> Tree a -> Bool
search _ Leaf = False
search element (Node left (x :| _) right)
  | element == x = True
  | element < x = search element left
  | otherwise = search element right

-- | Inserts the element into tree
insertX :: (Ord a) => a -> Tree a -> Tree a
insertX x Leaf = Node Leaf (x :| []) Leaf
insertX element (Node left list right)
  | element == searched = Node left (element <| list) right
  | element < searched = Node (insertX element left) list right
  | otherwise = Node left list (insertX element right)
  where searched = NonEmpty.head list

-- | Transforms the list into tree
fromListToTree :: (Ord a) => [a] -> Tree a
fromListToTree = foldr insertX Leaf

-- | Deletes the element from tree
delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete element (Node left list right)
  | element < searched = Node (delete element left) list right
  | element > searched = Node left list (delete element right)
  | NonEmpty.length list > 1 = Node left updatedList right
  | left /= Leaf && right /= Leaf = Node left (minElem right) (delete (NonEmpty.head $ minElem right) right)
  | left /= Leaf = left
  | right /= Leaf = right
  | otherwise = Leaf
  where
    searched = NonEmpty.head list
    updatedList = NonEmpty.fromList $ NonEmpty.tail list
    minElem :: Tree a -> NonEmpty a
    minElem Leaf                = undefined
    minElem (Node Leaf elems _) = elems
    minElem (Node leftTree _ _) = minElem leftTree
