{-# LANGUAGE InstanceSigs #-}
module Block2Task1 where

data Tree a = Nil | Node a (Tree a) (Tree a)
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Nil = acc
  foldr f acc (Node a left right) = foldr f updated' left
     where
     updated'  = f a updated''
     updated'' = foldr f acc right
  foldMap :: (Monoid b) => (a -> b) -> Tree a -> b
  foldMap _ Nil = mempty
  foldMap f (Node a left right) = foldMap f left <> f a <> foldMap f right
