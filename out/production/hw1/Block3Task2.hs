{-# LANGUAGE InstanceSigs #-}
module Block3Task2 where



data ThisOrThat a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) (Both x y) (This a)       = Both (x <> a) y
  (<>) (Both x y) (That b)       = Both x (y <> b)
  (<>) (Both x1 y1) (Both x2 y2) = Both (x1 <> x2) (y1 <> y2)
  (<>) (This a) (This b)         = This (a <> b)
  (<>) (This a) (That b)         = Both a b
  (<>) (That a) (This b)         = Both b a
  (<>) (That a) (That b)         = That (a <> b)
  (<>) (This a) (Both x y)       = Both (a <> x) y
  (<>) (That b) (Both x y)       = Both x (b <> y)


data NonEmpty a =
  a :| [a]
  deriving (Show)


instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y : ys))
