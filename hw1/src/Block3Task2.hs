{-# LANGUAGE InstanceSigs #-}

module Block3Task2
  ( NonEmpty(..)
  , ThisOrThat(..)
  ) where
-- | Data class for one, another or both data
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) (Both x y) (This a) = Both (x <> a) y
  (<>) (Both x y) (That b) = Both x (y <> b)
  (<>) (Both x1 y1) (Both x2 y2) = Both (x1 <> x2) (y1 <> y2)
  (<>) (This x) (This y) = This (x <> y)
  (<>) (This x) (That y) = Both x y
  (<>) (That x) (This y) = Both y x
  (<>) (That x) (That y) = That (x <> y)
  (<>) (This a) (Both x y) = Both (a <> x) y
  (<>) (That b) (Both x y) = Both x (b <> y)

-- | Data class for non-empty list
data NonEmpty a =
  a :| [a]
  deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y : ys))