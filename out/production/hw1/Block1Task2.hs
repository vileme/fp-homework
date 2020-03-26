{-# LANGUAGE InstanceSigs #-}
module Block1Task2 where

data Nat = Z | S Nat deriving (Show)

instance Enum Nat where
  toEnum :: Int -> Nat
  toEnum x | x < 0 = error "No state for negative numbers"
  toEnum 0 = Z
  toEnum x = S .  toEnum $ x - 1

  fromEnum :: Nat -> Int
  fromEnum Z     = 0
  fromEnum (S x) = fromEnum x + 1

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) x y = fromEnum x == fromEnum y

instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  x `compare` y = fromEnum x `compare` fromEnum y
  
add :: Nat -> Nat -> Nat
add x Z     = x
add x (S y) = S (add x y)

multiply :: Nat -> Nat -> Nat
multiply _ Z     = Z
multiply x (S y) = add x (multiply x y)

subtraction :: Nat -> Nat -> Nat
subtraction Z Z         = Z
subtraction x Z         = x
subtraction Z _         = error "The result isn't a natural number"
subtraction (S x) (S y) = subtraction x y

checkEquality :: Nat -> Nat -> Bool
checkEquality x y = subtraction x y == Z

