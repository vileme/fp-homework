module Block5Task1 where


import Control.Monad

data Expression
  = Constant Int
  | Sum Expression Expression
  | Subtract Expression Expression
  | Multiply Expression Expression
  | Div Expression Expression 
  | Pow Expression Expression
  deriving Show
  
data ArithmeticError
  = DividedByZero
  | PowToNegative
  deriving (Show)

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
    checkPow _ y | y < 0 = Left PowToNegative
    checkPow x y = Right $ x ^ y
