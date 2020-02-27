module Task3
  ( composition
  , contraction
  , identity
  , permutation
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | Composition function which behaves like "." operator
-- From types theory course we know that
-- B = S (K S) K, where K is actually const
composition :: (b -> c) -> (a -> b) -> a -> c
composition f = s (const f)

-- | ID function that returns an argument itself
-- Also from types theory course :
-- It's I combinator which defined from W like
-- I = W const, where W = S S (S const)
-- or contraction down below
identity :: a -> a
identity = contraction const

-- | It's also  Haskell Curry's W combinator
-- W = S S (S K)
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

-- | Haskell Curry's C combinator
-- which defined like C = S (S (K (S (K S) K)) S) (K K)
-- where k == const
permutation :: (a -> b -> c) -> b -> a -> c
permutation f x = s f (const x)
