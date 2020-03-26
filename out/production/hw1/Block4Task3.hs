{-# LANGUAGE InstanceSigs #-}
module Block4Task3 where



data NonEmpty a = a :| [a] deriving Show

headNonEmpty :: NonEmpty a -> a
headNonEmpty (x :| _) = x

tailNonEmpty :: NonEmpty a -> [a]
tailNonEmpty (_ :| xs) = xs

toListNonEmpty :: NonEmpty a -> [a]
toListNonEmpty (x :| xs) = x : xs
instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure a = a :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (f :| fs) <*> (x :| xs) = f x :| ((f <$> xs) ++ (fs <*> (x : xs)))

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| xs) f = headNonEmpty applied :| (tailNonEmpty applied ++ (xs >>= toListNonEmpty . f))
        where applied = f x


instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f acc (x :| xs) = f x $ foldr f acc xs

instance Traversable NonEmpty where
  traverse :: (Applicative f) => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs


