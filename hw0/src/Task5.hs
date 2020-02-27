module Task5
  ( churchMult
  , churchPlus
  , churchToInt
  , succChurch
  , zero
  ) where

type Nat a = (a -> a) -> a -> a

-- | Defines zero is Church encoding
zero :: Nat a
zero _ x = x

-- | Defines (+ 1) in terms of next
-- number in Church numerals
succChurch :: Nat a -> Nat a
succChurch n s z = s (n s z)

-- | Define plus in Church encoding
-- Apply s function on z m times and then
-- apply s function on the result n times.
-- In mult case, we apply application n times.
churchPlus ::  Nat a -> Nat a -> Nat a
churchPlus n m s z = n s (m s z)

-- | Define multiply in Church encoding.
churchMult :: Nat a -> Nat a -> Nat a
churchMult n m s = n (m s)

-- | Just take the Church numeral and do
-- n times (+ 1) from zero to get Integer
churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0
