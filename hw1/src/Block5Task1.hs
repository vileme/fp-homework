module Block5Task1
  ( ArithmeticError (..)
  , Expression(..)
  , eval
  ) where

import Control.Monad

-- | Data class which illustrates algebraic expression
data Expression
  = Constant Int
  | Sum Expression Expression
  | Subtract Expression Expression
  | Multiply Expression Expression
  | Div Expression Expression
  | Pow Expression Expression
  deriving (Show, Eq)

-- | Data class which illustrates arithmetic error
data ArithmeticError
  = DividedByZero
  | PowToNegative
  deriving (Show, Eq)

-- | Counts the value of arithmetic expression or returns error if there were such
eval :: Expression -> Either ArithmeticError Int
eval (Constant x) = Right x
eval (Sum left right) = join ((\x y -> Right (x + y)) <$> eval left <*> eval right)
eval (Subtract left right) = join ((\x y -> Right (x - y)) <$> eval left <*> eval right)
eval (Multiply left right) = join ((\x y -> Right (x * y)) <$> eval left <*> eval right)
eval (Div left right) = join (checkDiv <$> eval left <*> eval right)
  where
    checkDiv :: Int -> Int -> Either ArithmeticError Int
    checkDiv _ 0 = Left DividedByZero
    checkDiv x y = Right $ x `div` y
eval (Pow left right) = join (checkPow <$> eval left <*> eval right)
  where
    checkPow :: Int -> Int -> Either ArithmeticError Int
    checkPow _ y
      | y < 0 = Left PowToNegative
    checkPow x y = Right $ x ^ y