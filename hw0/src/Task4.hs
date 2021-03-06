module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import  Data.Function (fix)

-- | Recursive generation of infinite list,
-- which contains the same element.
iterateElement :: a -> [a]
iterateElement = fix (\f x -> x : f x)

-- | Counts the fibonacci number of n
fibonacci :: Integer -> Integer
fibonacci = fix fibonacci'
  where
    fibonacci' :: (Integer -> Integer) -> Integer -> Integer
    fibonacci' _ 0 = 0
    fibonacci' _ 1 = 1
    fibonacci' f n = f (n - 1) + f (n - 2)

-- | Counts factorial of n 
factorial :: Integer -> Integer
factorial = fix factorial'
  where
    factorial' :: (Integer -> Integer) -> Integer -> Integer
    factorial' _ 0 = 1
    factorial' f n = n * f (n - 1)

-- | Behaves like Prelude.map
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix mapFix'
  where
    mapFix' :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    mapFix' _ _ []       = []
    mapFix' f g (x : xs) = g x : f g xs
