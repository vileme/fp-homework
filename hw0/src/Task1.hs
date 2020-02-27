{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

-- | Actual proof of distributive property
-- prop> (A + (B * C)) == (A + B) * (A + C)
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

-- | Actual proof of associativity property
-- prop> A * (B * C) == (A * B) * C
associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

-- | Direct proof of left and right associativity
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (f, g)
  where
    f :: Either a (Either b c) -> Either (Either a b) c
    f (Left a)          = Left (Left a)
    f (Right (Left a))  = Left (Right a)
    f (Right (Right a)) = Right a
    g :: Either (Either a b) c -> Either a (Either b c)
    g (Left (Left a))   = Left a
    g (Left (Right a))  = Right (Left a)
    g (Right a)         = Right (Right a)
