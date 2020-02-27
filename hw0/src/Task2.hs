module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import  Data.Void (Void)

type Neg a = a -> Void

-- | Inhabited type of a -> !!a
-- Application of (a -> Void) and a
-- gives us Void, therefore our type is
-- a -> ((a -> Void) -> Void)
doubleNeg :: a -> Neg (Neg a)
doubleNeg a g = g a

-- | ((Either a a -> Void) -> Void) -> Void
-- | Inhabited type of !!(a|!a)
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg x = (x . Right) (x . Left)

-- | Can't inhabit type 
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | Can't inhabit type
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | Inhabited type of !!!a -> !a.
-- Got it from contrposition of a -> !!a
-- Just a reminder, contrposition looks like
-- a -> b \- !b -> !a
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = contrposition doubleNeg

contrposition :: (a -> b) -> Neg b -> Neg a
contrposition f g a = g $ f a
