{-# LANGUAGE InstanceSigs #-}
module Block4Task2 where

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)            = Leaf (f a)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f <*> tree = fmap f tree
  Branch left _ <*> Leaf a = left <*> Leaf a
  Branch t1 t2 <*> Branch t3 t4 = Branch (t1 <*> t3) (t2 <*> t4)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f acc (Leaf a)            = f a acc
  foldr f acc (Branch left right) = foldr f (foldr f acc right) left


instance Traversable Tree where
   traverse :: (Functor f,Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
   traverse f (Leaf a) = fmap Leaf (f a)
   traverse f (Branch left right) = fmap Branch (traverse f left) <*> traverse f right


