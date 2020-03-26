{-# LANGUAGE InstanceSigs #-}

module Block1Task2
  ( Nat(..)
  , add
  , checkEquality
  , fromInt
  , multiply
  , subtract
  , toInt
  ) where

-- | Data class which illustrates natural numbers
data Nat
  = Z
  | S Nat
  deriving (Show)

instance Enum Nat where
  toEnum :: Int -> Nat
  toEnum x
    | x < 0 = error "No state for negative numbers"
  toEnum 0 = Z
  toEnum x = S . toEnum $ x - 1
  fromEnum :: Nat -> Int
  fromEnum Z = 0
  fromEnum (S x) = fromEnum x + 1

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) x y = fromEnum x == fromEnum y

instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  x `compare` y = fromEnum x `compare` fromEnum y


-- | Sum of two natural numbers
add :: Nat -> Nat -> Nat
add x Z = x
add x (S y) = S (add x y)

-- | Multiplication of two natural numbers
multiply :: Nat -> Nat -> Nat
multiply _ Z = Z
multiply x (S y) = add x (multiply x y)

-- | Subtraction of two natural numbers
subtraction :: Nat -> Nat -> Nat
subtraction Z Z = Z
subtraction x Z = x
subtraction Z _ = error "The result isn't a natural number"
subtraction (S x) (S y) = subtraction x y

-- | Equality of two natural numbers
checkEquality :: Nat -> Nat -> Bool
checkEquality x y = subtraction x y == Z

-- | Transform Int to natural number
fromInt :: Int -> Nat
fromInt = toEnum

-- | Transform natural number to Int
toInt :: Nat -> Int
toInt = fromEnum
